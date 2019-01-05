module Multilinear.Generic.PtrTensor (
    Tensor(..), 
    _elemByElem, zipT, dot, 
    fromStorableTensor, toStorableTensor
) where

import           Control.DeepSeq
import qualified Control.Parallel.Strategies  as Parallel
import           Data.List
import qualified Data.Vector                  as Boxed
import qualified Data.Vector.Storable         as StorableV
import           Foreign.ForeignPtr
import           Foreign.ForeignPtr.Unsafe
import           Foreign.Ptr
import           Foreign.Storable
import           GHC.Generics
import           Multilinear.Class
import qualified Multilinear.Generic.Storable as StorableT
import qualified Multilinear.Index.Finite     as Finite
import qualified Multilinear.Index            as Index
import           System.IO.Unsafe


foreign import ccall "dot" 
    c_dot :: 
        Ptr Double -- ^ First array to dot
     -> Ptr Double -- ^ Second array to dot
     -> Int        -- ^ Length of arrays to dot
     -> Double     -- ^ Result dot product value

foreign import ccall "zip" 
    c_zip :: 
        FunPtr (Double -> Double) -- ^ Zipping combinator
     -> Ptr Double                -- ^ First array to zip
     -> Ptr Double                -- ^ Second array to zip
     -> Int                       -- ^ Length of arrays to zip
     -> Ptr Double                -- ^ Result array
     -> IO ()                     -- ^ return void

     
{-| ERROR MESSAGE -}
incompatibleTypes :: String
incompatibleTypes = "Incompatible tensor types!"

{-| ERROR MESSAGE -}
scalarIndices :: String
scalarIndices = "Scalar has no indices!"

-- | Tensor with Unboxed.Vector replaced by C array
data Tensor a where
    {-| Scalar -}
    Scalar :: {
        scalarVal :: a
    } -> Tensor a
    {-| Simple, one-dimensional finite tensor -}
    SimpleFinite :: {
        tensorFiniteIndex :: Finite.Index,
        tensorScalars     :: (Ptr a, Int)
    } -> Tensor a
    {-| Finite array of other tensors -}
    FiniteTensor :: {
        {-| Finite index "Mutltilinear.Index.Finite" of tensor -}
        tensorFiniteIndex :: Finite.Index,
        {-| Array of tensors on deeper recursion level -}
        tensorsFinite     :: Boxed.Vector (Tensor a)
    } -> Tensor a
    deriving (Eq, Generic)

fromStorableTensor :: Storable a => StorableT.Tensor a -> Tensor a
fromStorableTensor (StorableT.Scalar x) = Scalar x
fromStorableTensor (StorableT.SimpleFinite i ts) = let
    (ptr,_) = StorableV.unsafeToForeignPtr0 ts
    in SimpleFinite i (unsafeForeignPtrToPtr ptr, StorableV.length ts)
fromStorableTensor (StorableT.FiniteTensor i ts) = FiniteTensor i (fromStorableTensor <$> ts)

toStorableTensor :: Storable a => Tensor a -> StorableT.Tensor a
toStorableTensor (Scalar x) = StorableT.Scalar x
toStorableTensor (SimpleFinite i (ptr,len)) = let
    fptr = unsafePerformIO $ newForeignPtr_ ptr
    ts = StorableV.unsafeFromForeignPtr0 fptr len
    in StorableT.SimpleFinite i ts
toStorableTensor (FiniteTensor i ts) = StorableT.FiniteTensor i (toStorableTensor <$> ts)

-- | NFData instance
instance NFData a => NFData (Tensor a)

-- | Print tensor
-- | Assumes tensor is already in Multilinear class, because standardize function
instance (
    Show a, NFData a
    ) => Show (Tensor a) where
    show = show . toStorableTensor

{-| Apply a tensor operator (here denoted by (+) ) elem by elem, trying to connect as many common indices as possible -}
{-# INLINE _elemByElem #-}
_elemByElem :: 
                Tensor Double                                      -- ^ First argument of operator
             -> Tensor Double                                      -- ^ Second argument of operator
             -> (Double -> Double -> Double)                       -- ^ Operator on tensor elements if indices are different
             -> (Tensor Double -> Tensor Double -> Tensor Double)  -- ^ Tensor operator called if indices are the same
             -> Tensor Double                                      -- ^ Result tensor
-- @Scalar x + Scalar y = Scalar x + y@
_elemByElem (Scalar x1) (Scalar x2) f _ = Scalar $ f x1 x2
-- @Scalar x + Tensor t[i] = Tensor r[i] | r[i] = x + t[i]@
_elemByElem (Scalar x) t f _ = (x `f`) `Multilinear.Generic.PtrTensor.map` t
-- @Tensor t[i] + Scalar x = Tensor r[i] | r[i] = t[i] + x@
_elemByElem t (Scalar x) f _ = (`f` x) `Multilinear.Generic.PtrTensor.map` t
-- Two simple tensors case
_elemByElem t1@(SimpleFinite index1 (v1,len)) t2@(SimpleFinite index2 _) f op
    | Index.indexName index1 == Index.indexName index2 = op t1 t2
    | otherwise = let
        fptr1 = unsafePerformIO $ newForeignPtr_ v1
        ts1 = StorableV.unsafeFromForeignPtr0 fptr1 len
        in  FiniteTensor index1 $ Boxed.generate len
                (\i -> (\x -> f x `Multilinear.Generic.PtrTensor.map` t2) (ts1 StorableV.! i))
-- Two finite tensors case
_elemByElem t1@(FiniteTensor index1 v1) t2@(FiniteTensor index2 v2) f op
    | Index.indexName index1 == Index.indexName index2 = op t1 t2
    | Index.indexName index1 `Data.List.elem` indicesNames t2 =
        let len2 = Finite.indexSize index2
            rl = Boxed.toList $ (\x -> _elemByElem t1 x f op) <$> v2
            rlp = rl `Parallel.using` Parallel.parListChunk (len2 `div` 8) Parallel.rseq
        in  FiniteTensor index2 $ Boxed.fromList rlp
    | otherwise = 
        let len1 = Finite.indexSize index1
            rl = Boxed.toList $ (\x -> _elemByElem x t2 f op) <$> v1
            rlp = rl `Parallel.using` Parallel.parListChunk (len1 `div` 8) Parallel.rseq
        in  FiniteTensor index1 $ Boxed.fromList rlp
-- Simple and finite tensor case
_elemByElem t1@(SimpleFinite index1 _) t2@(FiniteTensor index2 v2) f op
    | Index.indexName index1 == Index.indexName index2 = op t1 t2
    | otherwise = 
        let len2 = Finite.indexSize index2
            rl = Boxed.toList $ (\x -> _elemByElem t1 x f op) <$> v2
            rlp = rl `Parallel.using` Parallel.parListChunk (len2 `div` 8) Parallel.rseq
        in  FiniteTensor index2 $ Boxed.fromList rlp
-- Finite and simple tensor case
_elemByElem t1@(FiniteTensor index1 v1) t2@(SimpleFinite index2 _) f op
    | Index.indexName index1 == Index.indexName index2 = op t1 t2
    | otherwise = 
        let len1 = Finite.indexSize index1
            rl = Boxed.toList $ (\x -> _elemByElem x t2 f op) <$> v1
            rlp = rl `Parallel.using` Parallel.parListChunk (len1 `div` 8) Parallel.rseq
        in  FiniteTensor index1 $ Boxed.fromList rlp

-- | Zipping two tensors with a combinator, assuming they have the same indices. 
{-# INLINE zipT #-}
zipT :: 
         (Double -> Double -> Double) -- ^ The zipping combinator
      -> Tensor Double                -- ^ First tensor to zip
      -> Tensor Double                -- ^ Second tensor to zip
      -> Tensor Double                -- ^ Result tensor
-- Two simple tensors case
zipT f t1@(SimpleFinite index1 (v1,len1)) t2@(SimpleFinite index2 (v2,len2)) = 
    if index1 == index2 then let
        fptr1 = unsafePerformIO $ newForeignPtr_ v1
        fptr2 = unsafePerformIO $ newForeignPtr_ v2
        (fptr3,len3) = StorableV.unsafeToForeignPtr0 $ StorableV.zipWith f 
                        (StorableV.unsafeFromForeignPtr0 fptr1 len1) 
                        (StorableV.unsafeFromForeignPtr0 fptr2 len2)
        in SimpleFinite index1 (unsafeForeignPtrToPtr fptr3,len3)
    else dot t1 t2
--Two finite tensors case
zipT f t1@(FiniteTensor index1 v1) t2@(FiniteTensor index2 v2)     = 
    if index1 == index2 then let
        l1l = Boxed.length v1
        l3 = Boxed.toList (Boxed.zipWith (zipT f) v1 v2) `Parallel.using` Parallel.parListChunk (l1l `div` 8) Parallel.rseq
        in FiniteTensor index1 $ Boxed.fromList l3
    else dot t1 t2
-- Zipping something with scalar is impossible
zipT _ _ _ = error $ "zipT: " ++ scalarIndices

-- | dot product of two tensors
{-# INLINE dot #-}
dot :: 
         Tensor Double  -- ^ First dot product argument
      -> Tensor Double  -- ^ Second dot product argument
      -> Tensor Double  -- ^ Resulting dot product
-- Two simple tensors product
dot (SimpleFinite i1@(Finite.Covariant count1 _) (ts1',len)) (SimpleFinite i2@(Finite.Contravariant count2 _) (ts2',_))
    | count1 == count2 = 
        Scalar $ c_dot ts1' ts2' len
    | otherwise = contractionErr "simple-simple" (Index.toTIndex i1) (Index.toTIndex i2)
dot (SimpleFinite i1@(Finite.Contravariant count1 _) (ts1',len)) (SimpleFinite i2@(Finite.Covariant count2 _) (ts2',_))
    | count1 == count2 = 
        Scalar $ c_dot ts1' ts2' len
    | otherwise = contractionErr "simple-simple" (Index.toTIndex i1) (Index.toTIndex i2)
dot t1@(SimpleFinite _ _) t2@(SimpleFinite _ _) = zipT (*) t1 t2
-- Two finite tensors product
dot (FiniteTensor i1@(Finite.Covariant count1 _) ts1') (FiniteTensor i2@(Finite.Contravariant count2 _) ts2')
    | count1 == count2 = 
        let zipList = Boxed.toList $ Boxed.zipWith (*) ts1' ts2' 
            zipListPar = zipList `Parallel.using` Parallel.parListChunk (count1 `div` 8) Parallel.rseq
        in  Boxed.sum $ Boxed.fromList zipListPar
    | otherwise = contractionErr "finite-finite" (Index.toTIndex i1) (Index.toTIndex i2)
dot (FiniteTensor i1@(Finite.Contravariant count1 _) ts1') (FiniteTensor i2@(Finite.Covariant count2 _) ts2')
    | count1 == count2 = 
        let zipList = Boxed.toList $ Boxed.zipWith (*) ts1' ts2' 
            zipListPar = zipList `Parallel.using` Parallel.parListChunk (count1 `div` 8) Parallel.rseq
        in  Boxed.sum $ Boxed.fromList zipListPar
    | otherwise = contractionErr "finite-finite" (Index.toTIndex i1) (Index.toTIndex i2)
dot t1@(FiniteTensor _ _) t2@(FiniteTensor _ _) = zipT (*) t1 t2
-- Other cases cannot happen!
dot t1' t2' = contractionErr "other" (head $ indices t1') (head $ indices t2')

-- | contraction error
{-# INLINE contractionErr #-}
contractionErr :: String         -- ^ dot function variant where error occured
               -> Index.TIndex   -- ^ Index of first dot product parameter
               -> Index.TIndex   -- ^ Index of second dot product parameter
               -> Tensor a       -- ^ Erorr message

contractionErr variant i1' i2' = error $
    "Tensor product: " ++ variant ++ " - " ++ incompatibleTypes ++
    " - index1 is " ++ show i1' ++
    " and index2 is " ++ show i2'

-- | Simple mapping
map :: (
    NFData a, NFData b, Storable a, Storable b
    ) => (a -> b)
      -> Tensor a
      -> Tensor b
map f x = case x of
    -- Mapping scalar simply maps its value
    Scalar v                -> Scalar $ f v
    -- Mapping complex tensor does mapping element by element
    SimpleFinite index (ptr,len)   -> let
        ts = StorableV.unsafeFromForeignPtr0 (unsafePerformIO $ newForeignPtr_ ptr) len
        ts' = f `StorableV.map` ts
        (fptr',len') = StorableV.unsafeToForeignPtr0 ts'
        ptr' = unsafeForeignPtrToPtr fptr'
        in SimpleFinite index (ptr',len')
    FiniteTensor index ts   -> 
        let len = Boxed.length ts
            lts = Boxed.toList $ Multilinear.Generic.PtrTensor.map f <$> ts
            ltsp = lts `Parallel.using` Parallel.parListChunk (len `div` 8) Parallel.rseq
        in  FiniteTensor index $ Boxed.fromList ltsp


-- | Tensors can be added, subtracted and multiplicated
instance Num (Tensor Double) where

    -- Adding - element by element
    {-# INLINE (+) #-}
    t1 + t2 = _elemByElem t1 t2 (+) $ zipT (+)

    -- Subtracting - element by element
    {-# INLINE (-) #-}
    t1 - t2 = _elemByElem t1 t2 (-) $ zipT (-)

    -- Multiplicating is treated as tensor product
    -- Tensor product applies Einstein summation convention
    {-# INLINE (*) #-}
    t1 * t2 = _elemByElem t1 t2 (*) dot

    -- Absolute value - element by element
    {-# INLINE abs #-}
    abs t = abs `Multilinear.Generic.PtrTensor.map` t

    -- Signum operation - element by element
    {-# INLINE signum #-}
    signum t = signum `Multilinear.Generic.PtrTensor.map` t

    -- Simple integer can be conveted to Scalar
    {-# INLINE fromInteger #-}
    fromInteger x = Scalar $ fromInteger x

-- Multilinear operations
instance Multilinear Tensor Double where
    fromIndices u d un dn f = 
        let st = Multilinear.Class.fromIndices u d un dn f :: StorableT.Tensor
        in fromStorableTensor st
  
    {-| Accessing tensor elements -}
    {-# INLINE el #-}
    -- Scalar has only one element
    el (Scalar x) _ = Scalar x
    -- simple tensor case
    el t1@(SimpleFinite index1 ts) (inds,vals) =
            -- zip indices with their given values
        let indvals = zip inds vals
            -- find value for simple tensor index if given
            val = Data.List.find (\(n,_) -> [n] == Index.indexName index1) indvals
            -- if value for current index is given
        in  if isJust val
            -- then get it from current tensor
            then Scalar $ ts StorableV.! snd (fromJust val)
            -- otherwise return whole tensor - no filtering defined
            else t1
    -- finite tensor case
    el (FiniteTensor index1 v1) (inds,vals) =
            -- zip indices with their given values
        let indvals = zip inds vals
            -- find value for current index if given
            val = Data.List.find (\(n,_) -> [n] == Index.indexName index1) indvals
            -- and remove used index from indices list
            indvals1 = Data.List.filter (\(n,_) -> [n] /= Index.indexName index1) indvals
            -- indices unused so far
            inds1 = Data.List.map fst indvals1
            -- and its corresponding values
            vals1 = Data.List.map snd indvals1
            -- if value for current index was given
        in  if isJust val
            -- then get it from current tensor and recursively process other indices
            then el (v1 Boxed.! snd (fromJust val)) (inds1,vals1)
            -- otherwise recursively access elements of all child tensors
            else FiniteTensor index1 $ (\t -> el t (inds,vals)) <$> v1

    -- List of all tensor indices
    {-# INLINE indices #-}
    indices x = case x of
        Scalar _            -> []
        FiniteTensor i ts   -> Index.toTIndex i : indices (ts Boxed.! 0)
        SimpleFinite i _    -> [Index.toTIndex i]

    -- Get tensor order [ (contravariant,covariant)-type ]
    {-# INLINE order #-}
    order x = case x of
        Scalar _ -> (0,0)
        SimpleFinite index _ -> case index of
            Finite.Contravariant _ _ -> (1,0)
            Finite.Covariant _ _     -> (0,1)
        FiniteTensor _ ts -> let (cnvr, covr) = order $ (ts Boxed.! 0)
             in case (head $ indices x) of
                Index.Contravariant _ _ -> (cnvr+1,covr)
                Index.Covariant _ _     -> (cnvr,covr+1)

    -- Get size of tensor index or Left if index is infinite or tensor has no such index
    {-# INLINE size #-}
    size t iname = case t of
        Scalar _             -> error scalarIndices
        SimpleFinite index _ -> 
            if Index.indexName index == iname 
            then Finite.indexSize index 
            else error indexNotFound
        FiniteTensor index ts -> 
            if Index.indexName index == iname
            then Finite.indexSize index
            else size (ts Boxed.! 0) iname

    -- Rename tensor indices
    {-# INLINE ($|) #-}
    Scalar x $| _ = Scalar x
    SimpleFinite (Finite.Contravariant isize _) ts $| (u:_, _) = SimpleFinite (Finite.Contravariant isize [u]) ts
    SimpleFinite (Finite.Covariant isize _) ts $| (_, d:_) = SimpleFinite (Finite.Covariant isize [d]) ts
    FiniteTensor (Finite.Contravariant isize _) ts $| (u:us, ds) = FiniteTensor (Finite.Contravariant isize [u]) $ ($| (us,ds)) <$> ts
    FiniteTensor (Finite.Covariant isize _) ts $| (us, d:ds) = FiniteTensor (Finite.Covariant isize [d]) $ ($| (us,ds)) <$> ts
    t $| _ = t

    -- Raise an index
    {-# INLINE (/\) #-}
    Scalar x /\ _ = Scalar x
    FiniteTensor index ts /\ n
        | Index.indexName index == n =
            FiniteTensor (Finite.Contravariant (Finite.indexSize index) n) $ (/\ n) <$> ts
        | otherwise =
            FiniteTensor index $ (/\ n) <$> ts
    t1@(SimpleFinite index ts) /\ n
        | Index.indexName index == n =
            SimpleFinite (Finite.Contravariant (Finite.indexSize index) n) ts
        | otherwise = t1

    -- Lower an index
    {-# INLINE (\/) #-}
    Scalar x \/ _ = Scalar x
    FiniteTensor index ts \/ n
        | Index.indexName index == n =
            FiniteTensor (Finite.Covariant (Finite.indexSize index) n) $ (\/ n) <$> ts
        | otherwise =
            FiniteTensor index $ (\/ n) <$> ts
    t1@(SimpleFinite index ts) \/ n
        | Index.indexName index == n =
            SimpleFinite (Finite.Covariant (Finite.indexSize index) n) ts
        | otherwise = t1

    {-| Transpose a tensor (switch all indices types) -}
    {-# INLINE transpose #-}
    transpose (Scalar x) = Scalar x

    transpose (FiniteTensor (Finite.Covariant count name) ts) =
        FiniteTensor (Finite.Contravariant count name) (Multilinear.Class.transpose <$> ts)
    transpose (FiniteTensor (Finite.Contravariant count name) ts) =
        FiniteTensor (Finite.Covariant count name) (Multilinear.Class.transpose <$> ts)

    transpose (SimpleFinite (Finite.Covariant count name) ts) =
        SimpleFinite (Finite.Contravariant count name) ts
    transpose (SimpleFinite (Finite.Contravariant count name) ts) =
        SimpleFinite (Finite.Covariant count name) ts


    {-| Shift tensor index right -}
    {-| Moves given index one level deeper in recursion -}
    -- Scalar has no indices to shift
    Scalar x `shiftRight` _ = Scalar x
    -- Simple tensor has only one index which cannot be shifted
    t1@(SimpleFinite _ _) `shiftRight` _ = t1
    -- Finite tensor is shifted by converting to list and transposing it
    t1@(FiniteTensor index1 ts1) `shiftRight` ind
        -- We don't shift this index
        | Data.List.length (indicesNames t1) > 1 && Index.indexName index1 /= ind =
            FiniteTensor index1 $ (|>> ind) <$> ts1
        -- We found index to shift
        | Data.List.length (indicesNames t1) > 1 && Index.indexName index1 == ind =
                -- Next index
            let index2 = tensorFiniteIndex (ts1 Boxed.! 0)
                -- Elements to transpose
                dane = if isSimple (ts1 Boxed.! 0)
                       then (\un -> Boxed.generate (StorableV.length un) (\i -> Scalar $ un StorableV.! i)) <$> 
                            (tensorScalars <$> ts1)
                       else tensorsFinite <$> ts1
                result = FiniteTensor index2 $ FiniteTensor index1 <$> (_transpose dane)
            -- reconstruct tensor with transposed elements
            in  _mergeScalars result
        -- there is only one index and therefore it cannot be shifted
        | otherwise = t1
    
    -- | Move contravariant indices to lower recursion level
    standardize tens = foldl' (<<<|) tens $ Index.indexName <$> (Index.isContravariant `Prelude.filter` indices tens)

    -- | Filter tensor
    filter _ (Scalar x) = Scalar x
    filter f (SimpleFinite index ts) = 
        let iname = Finite.indexName' index
            ts' = (\i _ -> f iname i) `StorableV.ifilter` ts
        in  SimpleFinite index { Finite.indexSize = StorableV.length ts' } ts'
    filter f (FiniteTensor index ts) = 
        let iname = Finite.indexName' index
            ts' = Multilinear.Class.filter f <$> ((\i _ -> f iname i) `Boxed.ifilter` ts)
            ts'' = 
                (\case 
                    (SimpleFinite _ ts) -> not $ StorableV.null ts
                    (FiniteTensor _ ts) -> not $ Boxed.null ts
                    _ -> error $ "Filter: " ++ tensorOfScalars
                ) `Boxed.filter` ts'
        in  FiniteTensor index { Finite.indexSize = Boxed.length ts'' } ts''
