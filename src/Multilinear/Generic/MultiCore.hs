{-|
Module      : Multilinear.Generic.MultiCore
Description : Generic array tensor, with multicore/threads parallelism
Copyright   : (c) Artur M. Brodzki, 2018
License     : BSD3
Maintainer  : artur@brodzki.org
Stability   : experimental
Portability : Windows/POSIX

-}

module Multilinear.Generic.MultiCore (
    Tensor(..), (!),
    isScalar, isSimple, isFiniteTensor,
    dot, _elemByElem, contractionErr, tensorIndex, _standardize
) where

import           Control.DeepSeq
import           Data.Foldable
import           Data.List
import qualified Data.Vector                as Boxed
import qualified Data.Vector.Unboxed        as Unboxed
import           Multilinear.Class          as Multilinear
import           Multilinear.Generic
import qualified Multilinear.Index          as Index
import qualified Multilinear.Index.Finite   as Finite

{-| ERROR MESSAGE -}
incompatibleTypes :: String
incompatibleTypes = "Incompatible tensor types!"

{-| ERROR MESSAGE -}
scalarIndices :: String
scalarIndices = "Scalar has no indices!"

{-| Apply a tensor operator (here denoted by (+) ) elem by elem, trying to connect as many common indices as possible -}
{-# INLINE _elemByElem' #-}
_elemByElem' :: (Num a, Unboxed.Unbox a, NFData a)
             => Tensor a                            -- ^ First argument of operator
             -> Tensor a                            -- ^ Second argument of operator
             -> (a -> a -> a)                       -- ^ Operator on tensor elements if indices are different
             -> (Tensor a -> Tensor a -> Tensor a)  -- ^ Tensor operator called if indices are the same
             -> Tensor a                            -- ^ Result tensor

-- @Scalar x + Scalar y = Scalar x + y@
_elemByElem' (Scalar x1) (Scalar x2) f _ = Scalar $ f x1 x2
-- @Scalar x + Tensor t[i] = Tensor r[i] | r[i] = x + t[i]@
_elemByElem' (Scalar x) t f _ = (x `f`) `Multilinear.Generic.map` t
-- @Tensor t[i] + Scalar x = Tensor r[i] | r[i] = t[i] + x@
_elemByElem' t (Scalar x) f _ = (`f` x) `Multilinear.Generic.map` t

-- Two simple tensors case
_elemByElem' t1@(SimpleFinite index1 v1) t2@(SimpleFinite index2 _) f op
    | Index.indexName index1 == Index.indexName index2 = op t1 t2
    | otherwise = FiniteTensor index1 $ Boxed.generate (Unboxed.length v1) 
        (\i -> (\x -> f x `Multilinear.Generic.map` t2) (v1 Unboxed.! i))

-- Two finite tensors case
_elemByElem' t1@(FiniteTensor index1 v1) t2@(FiniteTensor index2 v2) f op
    | Index.indexName index1 == Index.indexName index2 = op t1 t2
    | Index.indexName index1 `Data.List.elem` indicesNames t2 =
        FiniteTensor index2 $ (\x -> _elemByElem' t1 x f op) <$> v2
    | otherwise = FiniteTensor index1 $ (\x -> _elemByElem' x t2 f op) <$> v1

-- Simple and finite tensor case
_elemByElem' t1@(SimpleFinite index1 _) t2@(FiniteTensor index2 v2) f op
    | Index.indexName index1 == Index.indexName index2 = op t1 t2
    | otherwise = FiniteTensor index2 $ (\x -> _elemByElem' t1 x f op) <$> v2

-- Finite and simple tensor case
_elemByElem' t1@(FiniteTensor index1 v1) t2@(SimpleFinite index2 _) f op
    | Index.indexName index1 == Index.indexName index2 = op t1 t2
    | otherwise = FiniteTensor index1 $ (\x -> _elemByElem' x t2 f op) <$> v1

{-| Apply a tensor operator elem by elem and merge scalars to simple tensor at the and -}
{-# INLINE _elemByElem #-}
_elemByElem :: (Num a, Unboxed.Unbox a, NFData a)
            => Tensor a                             -- ^ First argument of operator
            -> Tensor a                             -- ^ Second argument of operator
            -> (a -> a -> a)                        -- ^ Operator on tensor elements if indices are different
            -> (Tensor a -> Tensor a -> Tensor a)   -- ^ Tensor operator called if indices are the same
            -> Tensor a                             -- ^ Result tensor
_elemByElem t1 t2 f op = 
    let commonIndices = Data.List.filter (`Data.List.elem` indicesNames t2) $ indicesNames t1
        t1' = foldl' (|>>>) t1 commonIndices
        t2' = foldl' (|>>>) t2 commonIndices
    in _mergeScalars $ _elemByElem' t1' t2' f op

-- | Zipping two tensors with a combinator, assuming they have the same indices. 
{-# INLINE zipT #-}
zipT :: (
    Num a, NFData a, Unboxed.Unbox a
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
    if index1 == index2 then 
        FiniteTensor index1 $ Boxed.zipWith (zipT f) v1 v2 
    else dot t1 t2

-- Finite and simple tensor case
zipT f t1@(FiniteTensor index1 v1) t2@(SimpleFinite index2 v2)     = 
    if index1 == index2 then 
        let f' t s = (`f` s) `_map` t
        in  FiniteTensor index1 $ Boxed.generate (Finite.indexSize index1) (\i -> f' (v1 Boxed.! i) (v2 Unboxed.! i)) 
    else dot t1 t2

-- Simple and finite tensor case
zipT f t1@(SimpleFinite index1 v1) t2@(FiniteTensor index2 v2)     = 
    if index1 == index2 then 
        let f' s t = (s `f`) `_map` t
        in  FiniteTensor index1 $ Boxed.generate (Finite.indexSize index1) (\i -> f' (v1 Unboxed.! i) (v2 Boxed.! i))
    else dot t1 t2

-- Zipping something with scalar is impossible
zipT _ _ _ = error $ "zipT: " ++ scalarIndices

-- | dot product of two tensors
{-# INLINE dot #-}
dot :: (Num a, Unboxed.Unbox a, NFData a)
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
    | count1 == count2 = Boxed.sum $ Boxed.zipWith (*) ts1' ts2'
    | otherwise = contractionErr "finite-finite" (Index.toTIndex i1) (Index.toTIndex i2)
dot (FiniteTensor i1@(Finite.Contravariant count1 _) ts1') (FiniteTensor i2@(Finite.Covariant count2 _) ts2')
    | count1 == count2 = Boxed.sum $ Boxed.zipWith (*) ts1' ts2'
    | otherwise = contractionErr "finite-finite" (Index.toTIndex i1) (Index.toTIndex i2)
dot t1@(FiniteTensor _ _) t2@(FiniteTensor _ _) = zipT (*) t1 t2

-- Simple tensor and finite tensor product
dot (SimpleFinite i1@(Finite.Covariant count1 _) ts1') (FiniteTensor i2@(Finite.Contravariant count2 _) ts2')
    | count1 == count2 = Boxed.sum $ Boxed.generate count1 (\i -> (ts1' Unboxed.! i) *. (ts2' Boxed.! i))
    | otherwise = contractionErr "simple-finite" (Index.toTIndex i1) (Index.toTIndex i2)
dot (SimpleFinite i1@(Finite.Contravariant count1 _) ts1') (FiniteTensor i2@(Finite.Covariant count2 _) ts2')
    | count1 == count2 = Boxed.sum $ Boxed.generate count1 (\i -> (ts1' Unboxed.! i) *. (ts2' Boxed.! i))
    | otherwise = contractionErr "simple-finite" (Index.toTIndex i1) (Index.toTIndex i2)
dot t1@(SimpleFinite _ _) t2@(FiniteTensor _ _) = zipT (*) t1 t2

-- Finite tensor and simple tensor product
dot (FiniteTensor i1@(Finite.Covariant count1 _) ts1') (SimpleFinite i2@(Finite.Contravariant count2 _) ts2')
    | count1 == count2 = Boxed.sum $ Boxed.generate count1 (\i -> (ts1' Boxed.! i) .* (ts2' Unboxed.! i))
    | otherwise = contractionErr "finite-simple" (Index.toTIndex i1) (Index.toTIndex i2)
dot (FiniteTensor i1@(Finite.Contravariant count1 _) ts1') (SimpleFinite i2@(Finite.Covariant count2 _) ts2')
    | count1 == count2 = Boxed.sum $ Boxed.generate count1 (\i -> (ts1' Boxed.! i) .* (ts2' Unboxed.! i))
    | otherwise = contractionErr "finite-simple" (Index.toTIndex i1) (Index.toTIndex i2)
dot t1@(FiniteTensor _ _) t2@(SimpleFinite _ _) = zipT (*) t1 t2

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

-- | Tensors can be added, subtracted and multiplicated
instance (Unboxed.Unbox a, Num a, NFData a) => Num (Tensor a) where

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
    abs t = abs `Multilinear.Generic.map` t

    -- Signum operation - element by element
    {-# INLINE signum #-}
    signum t = signum `Multilinear.Generic.map` t

    -- Simple integer can be conveted to Scalar
    {-# INLINE fromInteger #-}
    fromInteger x = Scalar $ fromInteger x

-- | Tensors can be divided by each other
instance (Unboxed.Unbox a, Fractional a, NFData a) => Fractional (Tensor a) where

    {-# INLINE (/) #-}
    -- Scalar division return result of division of its values
    Scalar x1 / Scalar x2 = Scalar $ x1 / x2
    -- Tensor and scalar are divided value by value
    Scalar x1 / t2 = (x1 /) `Multilinear.Generic.map` t2
    t1 / Scalar x2 = (/ x2) `Multilinear.Generic.map` t1
    -- Two complex tensors cannot be (for now) simply divided
    -- // TODO - tensor division and inversion
    _ / _ = error "TODO"

    -- A scalar can be generated from rational number
    {-# INLINE fromRational #-}
    fromRational x = Scalar $ fromRational x

-- Real-number functions on tensors.
-- Function of tensor is tensor of function of its elements
-- E.g. exp [1,2,3,4] = [exp 1, exp2, exp3, exp4]
instance (Unboxed.Unbox a, Floating a, NFData a) => Floating (Tensor a) where

    {-| PI number -}
    {-# INLINE pi #-}
    pi = Scalar pi

    {-| Exponential function. (exp t)[i] = exp( t[i] ) -}
    {-# INLINE exp #-}
    exp t = exp `Multilinear.Generic.map` t

    {-| Natural logarithm. (log t)[i] = log( t[i] ) -}
    {-# INLINE log #-}
    log t = log `Multilinear.Generic.map` t

    {-| Sinus. (sin t)[i] = sin( t[i] ) -}
    {-# INLINE sin #-}
    sin t = sin `Multilinear.Generic.map` t

    {-| Cosinus. (cos t)[i] = cos( t[i] ) -}
    {-# INLINE cos #-}
    cos t = cos `Multilinear.Generic.map` t

    {-| Inverse sinus. (asin t)[i] = asin( t[i] ) -}
    {-# INLINE asin #-}
    asin t = asin `Multilinear.Generic.map` t

    {-| Inverse cosinus. (acos t)[i] = acos( t[i] ) -}
    {-# INLINE acos #-}
    acos t = acos `Multilinear.Generic.map` t

    {-| Inverse tangent. (atan t)[i] = atan( t[i] ) -}
    {-# INLINE atan #-}
    atan t = atan `Multilinear.Generic.map` t

    {-| Hyperbolic sinus. (sinh t)[i] = sinh( t[i] ) -}
    {-# INLINE sinh #-}
    sinh t = sinh `Multilinear.Generic.map` t

    {-| Hyperbolic cosinus. (cosh t)[i] = cosh( t[i] ) -}
    {-# INLINE cosh #-}
    cosh t = cosh `Multilinear.Generic.map` t

    {-| Inverse hyperbolic sinus. (asinh t)[i] = asinh( t[i] ) -}
    {-# INLINE asinh #-}
    asinh t = acosh `Multilinear.Generic.map` t

    {-| Inverse hyperbolic cosinus. (acosh t)[i] = acosh (t[i] ) -}
    {-# INLINE acosh #-}
    acosh t = acosh `Multilinear.Generic.map` t

    {-| Inverse hyperbolic tangent. (atanh t)[i] = atanh( t[i] ) -}
    {-# INLINE atanh #-}
    atanh t = atanh `Multilinear.Generic.map` t

