module Multilinear.Generic.PtrTensor (
    Tensor(..), 
    _elemByElem, zipT, dot
) where

import           Control.DeepSeq
import qualified Control.Parallel.Strategies as Parallel
import           Data.List
import qualified Data.Vector                 as Boxed
import qualified Data.Vector.Storable        as StorableV
import           Foreign.ForeignPtr
import           Foreign.ForeignPtr.Unsafe
import           Foreign.Ptr
import           Foreign.Storable
import           GHC.Generics
--import           Multilinear.Class
import qualified Multilinear.Index.Finite    as Finite
import qualified Multilinear.Index           as Index
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

{-| Return generic tensor index -}
{-# INLINE tensorIndex #-}
tensorIndex :: Tensor a -> Index.TIndex
tensorIndex x = case x of
    Scalar _           -> error scalarIndices
    SimpleFinite i _   -> Index.toTIndex i
    FiniteTensor i _   -> Index.toTIndex i

indices :: Tensor a -> [Index.TIndex]
indices x = case x of
        Scalar _            -> []
        FiniteTensor i ts   -> Index.toTIndex i : indices (head $ Boxed.toList ts)
        SimpleFinite i _    -> [Index.toTIndex i]

{-| List of tensor indices names -}
{-# INLINE indicesNames #-}
indicesNames :: Tensor a -> [String]
indicesNames t = Index.indexName <$> indices t

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
dot t1' t2' = contractionErr "other" (tensorIndex t1') (tensorIndex t2')

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
