{-|
Module      : Multilinear.Generic.MultiCore
Description : Generic implementation of tensor as nested arrays, evaluated in sequential manner
Copyright   : (c) Artur M. Brodzki, 2018
License     : BSD3
Maintainer  : artur@brodzki.org
Stability   : experimental
Portability : Windows/POSIX

-}

module Multilinear.Generic.MultiCore (
    -- * Generic tensor datatype and its instances
    Tensor(..), 
    -- * Auxiliary functions
    (!), isScalar, isSimple, isFiniteTensor,
    tensorIndex, _mergeScalars, 
    _contractedIndices, _elemByElem, zipT,
    -- * Additional functions
    (.+), (.-), (.*), (+.), (-.), (*.),
    Multilinear.Generic.MultiCore.map, 
    Multilinear.Generic.MultiCore.filter,
    Multilinear.Generic.MultiCore.filterIndex,
    Multilinear.Generic.MultiCore.zipWith
) where

import           Control.DeepSeq
import qualified Control.Parallel.Strategies as Parallel
import           Data.Foldable
import           Data.List
import           Data.Maybe
import qualified Data.Set                    as Set
import qualified Data.Vector                 as Boxed
import qualified Data.Vector.Unboxed         as Unboxed
import           Foreign.Storable
import           GHC.Generics
import           Multilinear.Class           as Multilinear
import qualified Multilinear.Index           as Index
import qualified Multilinear.Index.Finite    as Finite

{-| ERROR MESSAGE -}
incompatibleTypes :: String
incompatibleTypes = "Incompatible tensor types!"

{-| ERROR MESSAGE -}
scalarIndices :: String
scalarIndices = "Scalar has no indices!"

{-| ERROR MESSAGE -}
indexNotFound :: String
indexNotFound = "This tensor has not such index!"

{-| ERROR MESSAGE -}
tensorOfScalars :: String
tensorOfScalars = "Tensor construction error! Vector of scalars"

{-| Tensor defined recursively as scalar or list of other tensors -}
{-| @c@ is type of a container, @i@ is type of index size and @a@ is type of tensor elements -}
data Tensor a where
    {-| Scalar -}
    Scalar :: {
        scalarVal :: a
    } -> Tensor a
    {-| Simple, one-dimensional finite tensor -}
    SimpleFinite :: {
        tensorFiniteIndex :: Finite.Index,
        tensorScalars     :: Unboxed.Vector a
    } -> Tensor a
    {-| Finite array of other tensors -}
    FiniteTensor :: {
        {-| Finite index "Mutltilinear.Index.Finite" of tensor -}
        tensorFiniteIndex :: Finite.Index,
        {-| Array of tensors on deeper recursion level -}
        tensorsFinite     :: Boxed.Vector (Tensor a)
    } -> Tensor a
    deriving (Eq, Generic)

{-| Return true if tensor is a scalar -}
{-# INLINE isScalar #-}
isScalar :: Unboxed.Unbox a => Tensor a -> Bool
isScalar x = case x of
    Scalar _ -> True
    _        -> False

{-| Return true if tensor is a simple tensor -}
{-# INLINE isSimple #-}
isSimple :: Unboxed.Unbox a => Tensor a -> Bool
isSimple x = case x of
    SimpleFinite _ _ -> True
    _                -> False

{-| Return True if tensor is a complex tensor -}
{-# INLINE isFiniteTensor #-}
isFiniteTensor :: Unboxed.Unbox a => Tensor a -> Bool
isFiniteTensor x = case x of
    FiniteTensor _ _ -> True
    _                -> False

{-| Return generic tensor index -}
{-# INLINE tensorIndex #-}
tensorIndex :: Unboxed.Unbox a => Tensor a -> Index.TIndex
tensorIndex x = case x of
    Scalar _           -> error scalarIndices
    SimpleFinite i _   -> Index.toTIndex i
    FiniteTensor i _   -> Index.toTIndex i

{-| Returns sample tensor on deeper recursion level.Used to determine some features common for all tensors -}
{-# INLINE firstTensor #-}
firstTensor :: Unboxed.Unbox a => Tensor a -> Tensor a
firstTensor x = case x of
    FiniteTensor _ ts   -> Boxed.head ts
    _                   -> x

{-| Recursive indexing on list tensor. If index is greater than index size, performs modulo indexing
    @t ! i = t[i]@ -}
{-# INLINE (!) #-}
(!) :: Unboxed.Unbox a => Tensor a      -- ^ tensor @t@
    -> Int           -- ^ index @i@
    -> Tensor a      -- ^ tensor @t[i]@
t ! i = case t of
    Scalar _            -> error scalarIndices
    SimpleFinite ind ts -> Scalar $ ts Unboxed.! (i `mod` Finite.indexSize ind)
    FiniteTensor ind ts -> ts Boxed.! (i `mod` Finite.indexSize ind)

-- | NFData instance
instance NFData a => NFData (Tensor a)

-- | Print tensor
instance (
    Multilinear Tensor a, Show a, Unboxed.Unbox a, NFData a
    ) => Show (Tensor a) where

    -- merge errors first and then print whole tensor
    show = show' . standardize
        where
        show' x = case x of
            -- Scalar is showed simply as its value
            Scalar v -> show v
            -- SimpleFinite is shown dependent on its index...
            SimpleFinite index ts -> show index ++ "S: " ++ case index of
                -- If index is contravariant, show tensor components vertically
                Finite.Contravariant _ _ -> "\n" ++ tail (Unboxed.foldl' (\string e -> string ++ "\n  |" ++ show e) "" ts)
                -- If index is covariant or indifferent, show tensor compoments horizontally
                _                        -> "["  ++ tail (Unboxed.foldl' (\string e -> string ++ "," ++ show e) "" ts) ++ "]"
            -- FiniteTensor is shown dependent on its index...
            FiniteTensor index ts -> show index ++ "T: " ++ case index of
                -- If index is contravariant, show tensor components vertically
                Finite.Contravariant _ _ -> "\n" ++ tail (Boxed.foldl' (\string e -> string ++ "\n  |" ++ show e) "" ts)
                -- If index is covariant or indifferent, show tensor compoments horizontally
                _                        -> "["  ++ tail (Boxed.foldl' (\string e -> string ++ "," ++ show e) "" ts) ++ "]"

{-| Merge FiniteTensor of Scalars to SimpleFinite tensor for performance improvement -}
{-# INLINE _mergeScalars #-}
_mergeScalars :: Unboxed.Unbox a => Tensor a -> Tensor a
_mergeScalars x = case x of
    (FiniteTensor index1 ts1) -> case ts1 Boxed.! 0 of
        Scalar _ -> SimpleFinite index1 $ Unboxed.generate (Boxed.length ts1) (\i -> scalarVal (ts1 Boxed.! i))
        _        -> FiniteTensor index1 $ _mergeScalars <$> ts1
    _ -> x

{-| Transpose Vector of Vectors, analogous to Data.List.transpose function. It is assumed, that all vectors on deeper recursion level have the same length.  -}
_transpose :: (
    NFData a
    ) => Boxed.Vector (Boxed.Vector a)  -- ^ Vector of vectors to transpose
      -> Boxed.Vector (Boxed.Vector a)
_transpose v = 
    let outerS = Boxed.length v
        innerS = Boxed.length $ v Boxed.! 0
        l = Boxed.toList $ Boxed.generate innerS (\i -> Boxed.generate outerS $ \j -> v Boxed.! j Boxed.! i)
        lp = l `Parallel.using` Parallel.parListChunk (innerS `div` 8) Parallel.rdeepseq
    in  Boxed.fromList lp

-- | Contracted indices have to be consumed in result tensor.
_contractedIndices :: 
    Tensor Double -- ^ first tensor to contract
 -> Tensor Double -- ^ second tensor to contract
 -> Set.Set String
_contractedIndices t1 t2 = 
    let iContravariantNames1 = Set.fromList $ Index.indexName <$> (Index.isContravariant `Prelude.filter` indices t1)
        iCovariantNames1 = Set.fromList $ Index.indexName <$> (Index.isCovariant `Prelude.filter` indices t1)
        iContravariantNames2 = Set.fromList $ Index.indexName <$> (Index.isContravariant `Prelude.filter` indices t2)
        iCovariantNames2 = Set.fromList $ Index.indexName <$> (Index.isCovariant `Prelude.filter` indices t2)
    in  -- contracted are indices covariant in the first tensor and contravariant in the second
        Set.intersection iCovariantNames1 iContravariantNames2 `Set.union`
        -- or contravariant in the first tensor and covariant in the second
        Set.intersection iContravariantNames1 iCovariantNames2

{-| Apply a tensor operator (here denoted by (+) ) elem by elem, trying to connect as many common indices as possible -}
{-# INLINE _elemByElem' #-}
_elemByElem' :: (Num a, Multilinear Tensor a, NFData a)
             => Tensor a                            -- ^ First argument of operator
             -> Tensor a                            -- ^ Second argument of operator
             -> (a -> a -> a)                       -- ^ Operator on tensor elements if indices are different
             -> (Tensor a -> Tensor a -> Tensor a)  -- ^ Tensor operator called if indices are the same
             -> Tensor a                            -- ^ Result tensor
-- @Scalar x + Scalar y = Scalar x + y@
_elemByElem' (Scalar x1) (Scalar x2) f _ = Scalar $ f x1 x2
-- @Scalar x + Tensor t[i] = Tensor r[i] | r[i] = x + t[i]@
_elemByElem' (Scalar x) t f _ = (x `f`) `Multilinear.Generic.MultiCore.map` t
-- @Tensor t[i] + Scalar x = Tensor r[i] | r[i] = t[i] + x@
_elemByElem' t (Scalar x) f _ = (`f` x) `Multilinear.Generic.MultiCore.map` t
-- Two simple tensors case
_elemByElem' t1@(SimpleFinite index1 v1) t2@(SimpleFinite index2 _) f op
    | Index.indexName index1 == Index.indexName index2 = op t1 t2
    | otherwise = FiniteTensor index1 $ Boxed.generate (Unboxed.length v1) 
        (\i -> (\x -> f x `Multilinear.Generic.MultiCore.map` t2) (v1 Unboxed.! i))
-- Two finite tensors case
_elemByElem' t1@(FiniteTensor index1 v1) t2@(FiniteTensor index2 v2) f op
    | Index.indexName index1 == Index.indexName index2 = op t1 t2
    | Index.indexName index1 `Data.List.elem` indicesNames t2 =
        let len2 = Finite.indexSize index2
            rl = Boxed.toList $ (\x -> _elemByElem' t1 x f op) <$> v2
            rlp = rl `Parallel.using` Parallel.parListChunk (len2 `div` 8) Parallel.rdeepseq
        in  FiniteTensor index2 $ Boxed.fromList rlp
    | otherwise = 
        let len1 = Finite.indexSize index1
            rl = Boxed.toList $ (\x -> _elemByElem' x t2 f op) <$> v1
            rlp = rl `Parallel.using` Parallel.parListChunk (len1 `div` 8) Parallel.rdeepseq
        in  FiniteTensor index1 $ Boxed.fromList rlp
-- Simple and finite tensor case
_elemByElem' t1@(SimpleFinite index1 _) t2@(FiniteTensor index2 v2) f op
    | Index.indexName index1 == Index.indexName index2 = op t1 t2
    | otherwise = 
        let len2 = Finite.indexSize index2
            rl = Boxed.toList $ (\x -> _elemByElem' t1 x f op) <$> v2
            rlp = rl `Parallel.using` Parallel.parListChunk (len2 `div` 8) Parallel.rdeepseq
        in  FiniteTensor index2 $ Boxed.fromList rlp
-- Finite and simple tensor case
_elemByElem' t1@(FiniteTensor index1 v1) t2@(SimpleFinite index2 _) f op
    | Index.indexName index1 == Index.indexName index2 = op t1 t2
    | otherwise = 
        let len1 = Finite.indexSize index1
            rl = Boxed.toList $ (\x -> _elemByElem' x t2 f op) <$> v1
            rlp = rl `Parallel.using` Parallel.parListChunk (len1 `div` 8) Parallel.rdeepseq
        in  FiniteTensor index1 $ Boxed.fromList rlp

{-| Apply a tensor operator elem by elem and merge scalars to simple tensor at the and -}
{-# INLINE _elemByElem #-}
_elemByElem :: (Num a, Multilinear Tensor a, NFData a)
            => Tensor a                             -- ^ First argument of operator
            -> Tensor a                             -- ^ Second argument of operator
            -> (a -> a -> a)                        -- ^ Operator on tensor elements if indices are different
            -> (Tensor a -> Tensor a -> Tensor a)   -- ^ Tensor operator called if indices are the same
            -> Tensor a                             -- ^ Result tensor
_elemByElem t1 t2 f op = 
    let commonIndices = 
            if indices t1 /= indices t2 then
                Data.List.filter (`Data.List.elem` indicesNames t2) $ indicesNames t1
            else []
        t1' = foldl' (|>>>) t1 commonIndices
        t2' = foldl' (|>>>) t2 commonIndices
    in _mergeScalars $ _elemByElem' t1' t2' f op

-- | Zipping two tensors with a combinator, assuming they have the same indices. 
{-# INLINE zipT #-}
zipT :: (
    Num a, Multilinear Tensor a, NFData a
    ) => (a -> a -> a)                        -- ^ The zipping combinator
      -> Tensor a                             -- ^ First tensor to zip
      -> Tensor a                             -- ^ Second tensor to zip
      -> Tensor a                             -- ^ Result tensor
-- Two simple tensors case
zipT f t1@(SimpleFinite index1 v1) t2@(SimpleFinite index2 v2) = 
    if index1 == index2 then 
        SimpleFinite index1 $ Unboxed.zipWith f v1 v2 
    else dot t1 t2
--Two finite tensors case
zipT f t1@(FiniteTensor index1 v1) t2@(FiniteTensor index2 v2)     = 
    if index1 == index2 then let
        l1l = Boxed.length v1
        l3 = Boxed.toList (Boxed.zipWith (zipT f) v1 v2) `Parallel.using` Parallel.parListChunk (l1l `div` 8) Parallel.rdeepseq
        in FiniteTensor index1 $ Boxed.fromList l3
    else dot t1 t2
-- Zipping something with scalar is impossible
zipT _ _ _ = error $ "zipT: " ++ scalarIndices

-- | dot product of two tensors
{-# INLINE dot #-}
dot :: (Num a, Multilinear Tensor a, NFData a)
      => Tensor a  -- ^ First dot product argument
      -> Tensor a  -- ^ Second dot product argument
      -> Tensor a  -- ^ Resulting dot product
-- Two simple tensors product
dot (SimpleFinite i1@(Finite.Covariant count1 _) ts1') (SimpleFinite i2@(Finite.Contravariant count2 _) ts2')
    | count1 == count2 = 
        Scalar $ Unboxed.sum $ Unboxed.zipWith (*) ts1' ts2'
    | otherwise = contractionErr "simple-simple" (Index.toTIndex i1) (Index.toTIndex i2)
dot (SimpleFinite i1@(Finite.Contravariant count1 _) ts1') (SimpleFinite i2@(Finite.Covariant count2 _) ts2')
    | count1 == count2 = 
        Scalar $ Unboxed.sum $ Unboxed.zipWith (*) ts1' ts2'
    | otherwise = contractionErr "simple-simple" (Index.toTIndex i1) (Index.toTIndex i2)
dot t1@(SimpleFinite _ _) t2@(SimpleFinite _ _) = zipT (*) t1 t2
-- Two finite tensors product
dot (FiniteTensor i1@(Finite.Covariant count1 _) ts1') (FiniteTensor i2@(Finite.Contravariant count2 _) ts2')
    | count1 == count2 = 
        let zipList = Boxed.toList $ Boxed.zipWith (*) ts1' ts2' 
            zipListPar = zipList `Parallel.using` Parallel.parListChunk (count1 `div` 8) Parallel.rdeepseq
        in  Boxed.sum $ Boxed.fromList zipListPar
    | otherwise = contractionErr "finite-finite" (Index.toTIndex i1) (Index.toTIndex i2)
dot (FiniteTensor i1@(Finite.Contravariant count1 _) ts1') (FiniteTensor i2@(Finite.Covariant count2 _) ts2')
    | count1 == count2 = 
        let zipList = Boxed.toList $ Boxed.zipWith (*) ts1' ts2' 
            zipListPar = zipList `Parallel.using` Parallel.parListChunk (count1 `div` 8) Parallel.rdeepseq
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

-- | zipping error
{-# INLINE zipErr #-}
zipErr :: String         -- ^ zipT function variant where the error occured
       -> Index.TIndex   -- ^ Index of first dot product parameter
       -> Index.TIndex   -- ^ Index of second dot product parameter
       -> Tensor a       -- ^ Erorr message
zipErr variant i1' i2' = error $
    "zipT: " ++ variant ++ " - " ++ incompatibleTypes ++
    " - index1 is " ++ show i1' ++
    " and index2 is " ++ show i2'

-- | Tensors can be added, subtracted and multiplicated
instance (Num a, Multilinear Tensor a, NFData a) => Num (Tensor a) where

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
    abs t = abs `Multilinear.Generic.MultiCore.map` t

    -- Signum operation - element by element
    {-# INLINE signum #-}
    signum t = signum `Multilinear.Generic.MultiCore.map` t

    -- Simple integer can be conveted to Scalar
    {-# INLINE fromInteger #-}
    fromInteger x = Scalar $ fromInteger x

-- | Tensors can be divided by each other
instance (Fractional a, Multilinear Tensor a, NFData a) => Fractional (Tensor a) where
    -- Tensor dividing: TODO
    {-# INLINE (/) #-}
    _ / _ = error "TODO"

    -- A scalar can be generated from rational number
    {-# INLINE fromRational #-}
    fromRational x = Scalar $ fromRational x

-- Real-number functions on tensors.
-- Function of tensor is tensor of function of its elements
-- E.g. exp [1,2,3,4] = [exp 1, exp2, exp3, exp4]
instance (Floating a, Multilinear Tensor a, NFData a) => Floating (Tensor a) where

    {-| PI number -}
    {-# INLINE pi #-}
    pi = Scalar pi

    {-| Exponential function. (exp t)[i] = exp( t[i] ) -}
    {-# INLINE exp #-}
    exp t = exp `Multilinear.Generic.MultiCore.map` t

    {-| Natural logarithm. (log t)[i] = log( t[i] ) -}
    {-# INLINE log #-}
    log t = log `Multilinear.Generic.MultiCore.map` t

    {-| Sinus. (sin t)[i] = sin( t[i] ) -}
    {-# INLINE sin #-}
    sin t = sin `Multilinear.Generic.MultiCore.map` t

    {-| Cosinus. (cos t)[i] = cos( t[i] ) -}
    {-# INLINE cos #-}
    cos t = cos `Multilinear.Generic.MultiCore.map` t

    {-| Inverse sinus. (asin t)[i] = asin( t[i] ) -}
    {-# INLINE asin #-}
    asin t = asin `Multilinear.Generic.MultiCore.map` t

    {-| Inverse cosinus. (acos t)[i] = acos( t[i] ) -}
    {-# INLINE acos #-}
    acos t = acos `Multilinear.Generic.MultiCore.map` t

    {-| Inverse tangent. (atan t)[i] = atan( t[i] ) -}
    {-# INLINE atan #-}
    atan t = atan `Multilinear.Generic.MultiCore.map` t

    {-| Hyperbolic sinus. (sinh t)[i] = sinh( t[i] ) -}
    {-# INLINE sinh #-}
    sinh t = sinh `Multilinear.Generic.MultiCore.map` t

    {-| Hyperbolic cosinus. (cosh t)[i] = cosh( t[i] ) -}
    {-# INLINE cosh #-}
    cosh t = cosh `Multilinear.Generic.MultiCore.map` t

    {-| Inverse hyperbolic sinus. (asinh t)[i] = asinh( t[i] ) -}
    {-# INLINE asinh #-}
    asinh t = acosh `Multilinear.Generic.MultiCore.map` t

    {-| Inverse hyperbolic cosinus. (acosh t)[i] = acosh (t[i] ) -}
    {-# INLINE acosh #-}
    acosh t = acosh `Multilinear.Generic.MultiCore.map` t

    {-| Inverse hyperbolic tangent. (atanh t)[i] = atanh( t[i] ) -}
    {-# INLINE atanh #-}
    atanh t = atanh `Multilinear.Generic.MultiCore.map` t

-- Multilinear operations
instance (NFData a, Unboxed.Unbox a, Storable a, NFData a) => Multilinear Tensor a where
    -- Generic tensor constructor
    -- If only one upper index is given, generate a SimpleFinite tensor with upper index
    fromIndices [u] [] [s] [] f = 
        SimpleFinite (Finite.Contravariant s [u]) $ Unboxed.generate s $ \x -> f [x] []
  
    -- If only one lower index is given, generate a SimpleFinite tensor with lower index
    fromIndices [] [d] [] [s] f = 
        SimpleFinite (Finite.Covariant s [d]) $ Unboxed.generate s $ \x -> f [] [x]
  
    -- If many indices are given, first generate upper indices recursively from indices list
    fromIndices (u:us) d (s:size) dsize f =
        FiniteTensor (Finite.Contravariant s [u]) $ Boxed.generate s (\x -> fromIndices us d size dsize (\uss dss -> f (x:uss) dss) )
  
    -- After upper indices, generate lower indices recursively from indices list
    fromIndices u (d:ds) usize (s:size) f =
        FiniteTensor (Finite.Covariant s [d]) $ Boxed.generate s (\x -> fromIndices u ds usize size (\uss dss -> f uss (x:dss)) )
  
    -- If there are indices without size or sizes without names, throw an error
    fromIndices _ _ _ _ _ = error "Indices and its sizes incompatible!"
  
    {-| Accessing tensor elements -}
    {-# INLINE el #-}
    -- Scalar has only one element
    el (Scalar x) _ = Scalar x
    -- simple tensor case
    el t1@(SimpleFinite index1 _) (inds,vals) =
            -- zip indices with their given values
        let indvals = zip inds vals
            -- find value for simple tensor index if given
            val = Data.List.find (\(n,_) -> [n] == Index.indexName index1) indvals
            -- if value for current index is given
        in  if isJust val
            -- then get it from current tensor
            then t1 ! snd (fromJust val)
            -- otherwise return whole tensor - no filtering defined
            else t1
    -- finite tensor case
    el t1@(FiniteTensor index1 v1) (inds,vals) =
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
            then el (t1 ! snd (fromJust val)) (inds1,vals1)
            -- otherwise recursively access elements of all child tensors
            else FiniteTensor index1 $ (\t -> el t (inds,vals)) <$> v1

    -- List of all tensor indices
    {-# INLINE indices #-}
    indices x = case x of
        Scalar _            -> []
        FiniteTensor i ts   -> Index.toTIndex i : indices (head $ toList ts)
        SimpleFinite i _    -> [Index.toTIndex i]

    -- Get tensor order [ (contravariant,covariant)-type ]
    {-# INLINE order #-}
    order x = case x of
        Scalar _ -> (0,0)
        SimpleFinite index _ -> case index of
            Finite.Contravariant _ _ -> (1,0)
            Finite.Covariant _ _     -> (0,1)
        _ -> let (cnvr, covr) = order $ firstTensor x
             in case tensorIndex x of
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
        FiniteTensor index _ -> 
            if Index.indexName index == iname
            then Finite.indexSize index
            else size (firstTensor t) iname

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
        FiniteTensor (Finite.Contravariant count name) (Multilinear.transpose <$> ts)
    transpose (FiniteTensor (Finite.Contravariant count name) ts) =
        FiniteTensor (Finite.Covariant count name) (Multilinear.transpose <$> ts)

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
                       then (\un -> Boxed.generate (Unboxed.length un) (\i -> Scalar $ un Unboxed.! i)) <$> 
                            (tensorScalars <$> ts1)
                       else tensorsFinite <$> ts1
                result = FiniteTensor index2 $ FiniteTensor index1 <$> (_transpose dane)
            -- reconstruct tensor with transposed elements
            in  _mergeScalars result
        -- there is only one index and therefore it cannot be shifted
        | otherwise = t1
    
    -- | Move contravariant indices to lower recursion level
    standardize tens = foldl' (<<<|) tens $ Index.indexName <$> (Index.isContravariant `Prelude.filter` indices tens)

-- Add scalar right
{-# INLINE (.+) #-}
(.+) :: (
    Num a, Multilinear Tensor a, NFData a
    ) => Tensor a 
      -> a 
      -> Tensor a
t .+ x = (+x) `Multilinear.Generic.MultiCore.map` t

-- Subtract scalar right
{-# INLINE (.-) #-}
(.-) :: (
    Num a, Multilinear Tensor a, NFData a
    ) => Tensor a 
      -> a 
      -> Tensor a
t .- x = (\p -> p - x) `Multilinear.Generic.MultiCore.map` t

-- Multiplicate by scalar right
{-# INLINE (.*) #-}
(.*) :: (
    Num a, Multilinear Tensor a, NFData a
    ) => Tensor a 
      -> a 
      -> Tensor a
t .* x = (*x) `Multilinear.Generic.MultiCore.map` t

-- Add scalar left
{-# INLINE (+.) #-}
(+.) :: (
    Num a, Multilinear Tensor a, NFData a
    ) => a 
      -> Tensor a 
      -> Tensor a
x +. t = (x+) `Multilinear.Generic.MultiCore.map` t

-- Subtract scalar left
{-# INLINE (-.) #-}
(-.) :: (
    Num a, Multilinear Tensor a, NFData a
    ) => a 
      -> Tensor a 
      -> Tensor a
x -. t = (x-) `Multilinear.Generic.MultiCore.map` t

-- Multiplicate by scalar left
{-# INLINE (*.) #-}
(*.) :: (
    Num a, Multilinear Tensor a, NFData a
    ) => a 
      -> Tensor a 
      -> Tensor a
x *. t = (x*) `Multilinear.Generic.MultiCore.map` t

-- | Simple mapping
map :: (
    Multilinear Tensor a, Multilinear Tensor b, 
    NFData a, NFData b
    ) => (a -> b)
      -> Tensor a
      -> Tensor b
map f x = case x of
    -- Mapping scalar simply maps its value
    Scalar v                -> Scalar $ f v
    -- Mapping complex tensor does mapping element by element
    SimpleFinite index ts   -> SimpleFinite index (f `Unboxed.map` ts)
    FiniteTensor index ts   -> 
        let len = Boxed.length ts
            lts = Boxed.toList $ Multilinear.Generic.MultiCore.map f <$> ts
            ltsp = lts `Parallel.using` Parallel.parListChunk (len `div` 8) Parallel.rdeepseq
        in  FiniteTensor index $ Boxed.fromList ltsp

{-| Filtering tensor. 
    Filtering multi-dimensional arrray may be dangerous, as we always assume, 
    that on each recursion level, all tensors have the same size (length). 
    To disable invalid filters, filtering is done over indices, not tensor elements. 
    Filter function takes and index name and index value and if it returns True, this index value remains in result tensor. 
    This allows to remove whole columns or rows of eg. a matrix: 
        filter (\i n -> i /= "a" || i > 10) filters all rows of "a" index (because if i /= "a", filter returns True)
        and for "a" index filter elements with index value <= 10
    But this disallow to remove particular matrix element. 
    If for some index all elements are removed, the index itself is removed from tensor. -}
{-# INLINE filter #-}
filter :: (
    Multilinear Tensor a, NFData a
    ) => (String -> Int -> Bool) -- ^ filter function
      -> Tensor a                -- ^ tensor to filter
      -> Tensor a
filter _ (Scalar x) = Scalar x
filter f (SimpleFinite index ts) = 
    let iname = Finite.indexName' index
        ts' = (\i _ -> f iname i) `Unboxed.ifilter` ts
    in  SimpleFinite index { Finite.indexSize = Unboxed.length ts' } ts'
filter f (FiniteTensor index ts) = 
    let iname = Finite.indexName' index
        ts' = Multilinear.Generic.MultiCore.filter f <$> ((\i _ -> f iname i) `Boxed.ifilter` ts)
        ts'' = 
            (\case 
                (SimpleFinite _ ts) -> not $ Unboxed.null ts
                (FiniteTensor _ ts) -> not $ Boxed.null ts
                _ -> error $ "Filter: " ++ tensorOfScalars
            ) `Boxed.filter` ts'
    in  FiniteTensor index { Finite.indexSize = Boxed.length ts'' } ts''

{-| Filtering one index of tensor. -}
{-# INLINE filterIndex #-}
filterIndex :: (
    Multilinear Tensor a, NFData a
    ) => String        -- ^ Index name to filter
      -> (Int -> Bool) -- ^ filter function
      -> Tensor a      -- ^ tensor to filter
      -> Tensor a
filterIndex iname f = Multilinear.Generic.MultiCore.filter (\i n -> i /= iname || f n)

{-| Zip tensors with binary combinator, assuming they have all indices the same -}
{-# INLINE zipWith' #-}
zipWith' :: (
    Multilinear Tensor a, Multilinear Tensor b, Multilinear Tensor c, 
    NFData a, NFData b, NFData c
    ) => (a -> b -> c) 
      -> Tensor a 
      -> Tensor b 
      -> Tensor c
-- Zipping two Scalars simply combines their values 
zipWith' f (Scalar x1) (Scalar x2) = Scalar $ f x1 x2
-- zipping complex tensor with scalar 
zipWith' f t (Scalar x) = (`f` x) `Multilinear.Generic.MultiCore.map` t
-- zipping scalar with complex tensor
zipWith' f (Scalar x) t = (x `f`) `Multilinear.Generic.MultiCore.map` t
-- Two simple tensors case
zipWith' f (SimpleFinite index1 v1) (SimpleFinite index2 v2) = 
    if index1 == index2 then 
        SimpleFinite index1 $ Unboxed.zipWith f v1 v2 
    else zipErr "simple-simple" (Index.toTIndex index1) (Index.toTIndex index2)
--Two finite tensors case
zipWith' f (FiniteTensor index1 v1) (FiniteTensor index2 v2)     = 
    if index1 == index2 then 
        FiniteTensor index1 $ Boxed.zipWith (Multilinear.Generic.MultiCore.zipWith f) v1 v2 
    else zipErr "finite-finite" (Index.toTIndex index1) (Index.toTIndex index2)
-- Other cases cannot happen!
zipWith' _ _ _ = error "Invalid indices to peroform zip!"

{-# INLINE zipWith #-}
zipWith :: (
    Multilinear Tensor a, Multilinear Tensor b, Multilinear Tensor c, 
    NFData a, NFData b, NFData c
    ) => (a -> b -> c) 
      -> Tensor a 
      -> Tensor b 
      -> Tensor c
zipWith f t1 t2 = 
    let t1' = standardize t1
        t2' = standardize t2
    in  zipWith' f t1' t2'
