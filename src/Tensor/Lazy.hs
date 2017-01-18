-----------------------------------------------------------------------------
--
-- Module      :  Tensor.Lazy
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Tensor.Lazy (
    --Tensor(..)
) where
{-
import Data.IntMap.Lazy as I
import Tensor.Multilinear as T
import Tensor.Index
import Prelude as P
import Linear.Metric
import Linear.Vector

{- ERROR MESSAGES -}
noContravariant :: String
noContravariant = "No contravariant index here!"
noCovariant :: String
noCovariant = "No covariant index here!"
zeroElems :: String
zeroElems = "Tensor must contain at leat one element!"
incompatibleTypes :: String
incompatibleTypes = "Incompatible tensor types!"
indexListIncompatible :: String
indexListIncompatible = "List length must be the same as index size!"


{- RESTRICTED TENSOR DATATYPE -}

data Tensor a =
    Scalar { scalarVal :: a } |
    Tensor {
        tensorIndex :: TIndexType,
        tensorData :: IntMap (Tensor a)
    }
    deriving Eq

instance Show a => Show (Tensor a) where
    show (Scalar x) = show x
    show (Tensor index ts) = "Tensor (" P.++ show index P.++ ") " P.++ show ts

-- Tensor is a Functor instance
instance Functor Tensor where
    fmap f (Scalar v_) = Scalar (f v_)
    fmap f (Tensor indexT ts) = Tensor indexT v2
        where v2 = fmap (fmap f) ts

-- Tensor operations
instance Multilinear Tensor  where
    -- Unsafe contravariant indexing
    (Tensor (Contravariant _ _) tens_) !^ ind = tens_ ! ind
    _ !^ _ = error $ "(!^): " P.++ noContravariant

    -- Unsafe covariant indexing
    (Tensor (Covariant _ _) tens_) !/ ind = tens_ ! ind
    _ !/ _ = error $ "(!/): " P.++ noCovariant

    -- Safe contravariant indexing
    Nothing !^? _ = Nothing
    Just (Tensor (Contravariant _ _) tens_) !^? ind = tens_ !? ind
    _ !^? _ = Nothing

    -- Safe covariant indexing
    Nothing !/? _ = Nothing
    Just (Tensor (Covariant _ _) tens_) !/? ind = tens_ !? ind
    _ !/? _ = Nothing

    -- Tensor product with Einstein summation convention
    (Scalar x) !*! t2 = (x*) <$> t2
    t1 !*! (Scalar x) = (*x) <$> t1
    t1@(Tensor index1 ts1) !*! t2@(Tensor index2 ts2)
        | indexName index1 == indexName index2 = t1 `Tensor.Lazy.dot` t2
        | indexName index1 `P.elem` (indexName <$> indices t2) =
            T.generate index2 (\i -> t1 !*! (ts2 ! i))
        | indexName index2 `P.elem` (indexName <$> indices t1) =
            T.generate index1 (\i -> (ts1 ! i) !*! t2)
        | otherwise = T.generate index1 (\i -> (ts1 ! i) !*! t2)

    -- Generate tensor from generator function of indices
    generate index f
        | indexCount index > 0 = Tensor index $ I.generate (indexCount index) f
        | otherwise = error $ "generate: " P.++ zeroElems

    -- Create tensor by replicating another one
    replicate index tens_
        | indexCount index > 0 = Tensor index $ V.replicate (indexCount index) tens_
        | otherwise = error $ "replicate: " P.++ zeroElems

    -- Get tensor order [ (p,q)-type ]
    order (Scalar _) = (0,0)
    order (Tensor (Contravariant _ _) t) = (cnvr+1,covr)
        where (cnvr,covr) = order $ t ! 0
    order (Tensor (Covariant _ _) t) = (cnvr,covr+1)
        where (cnvr,covr) = order $ t ! 0

    -- Get number of elems in tensor
    elems (Scalar _) = 1
    elems (Tensor index ts) = indexCount index * T.elems (ts ! 0)

    -- Get list of all tensor indices
    indices (Scalar _) = []
    indices (Tensor index ts) = index : indices (ts ! 0)

    -- Rename tensor index
    rename (Scalar x) _ _ = Scalar x
    rename t@(Tensor (Contravariant count name) ts) from to
        | name == from = Tensor (Contravariant count to) ts
        | otherwise = t
    rename t@(Tensor (Covariant count name) ts) from to
        | name == from = Tensor (Covariant count to) ts
        | otherwise = t

    -- Transpose a tensor (switch all indices types)
    transpose (Scalar x) = Scalar x
    transpose (Tensor (Covariant count name) ts) = Tensor (Contravariant count name) (transpose <$> ts)
    transpose (Tensor (Contravariant count name) ts) = Tensor (Covariant count name) (transpose <$> ts)

    -- Transpose a tensor - switch only the first index
    transpose1 (Scalar x) = Scalar x
    transpose1 (Tensor (Covariant count name) ts) = Tensor (Contravariant count name) ts
    transpose1 (Tensor (Contravariant count name) ts) = Tensor (Covariant count name) ts

    -- Concatenation of tensor by given index or with creating a new one
    {-concat index s1@(Scalar _) s2@(Scalar _) = Tensor index $ V.fromList [s1,s2]
    concat index t1@(Tensor index1 ts1) t2@(Tensor index2 ts2)
        | index1 == index && index2 == index = Tensor index (ts1 V.++ ts2)
        | otherwise = -}

    -- Generate 1-rank tensor from a list
    fromList index ls
        | indexCount index == P.length ls = Tensor index $ I.fromList $ Scalar <$> ls
        | otherwise = error $ "fromList: " P.++ indexListIncompatible

    -- Generate 2-rank tensor from list of lists
    fromList2 index1 index2 ls
        | indexCount index1 == P.length ls = Tensor index1 $ I.fromList $ T.fromList index2 <$> ls
        | otherwise = error $ "fromListList: " P.++ indexListIncompatible

    -- Generate 3-rank tensor from a list
    fromList3 index1 index2 index3 ls
        | indexCount index1 == P.length ls = Tensor index1 $ I.fromList $ T.fromList2 index2 index3 <$> ls
        | otherwise = error $ "fromListList: " P.++ indexListIncompatible

-- Dot product of covector and vector (specifically in this order)
dot :: Num a => Tensor a -> Tensor a -> Tensor a
Scalar x1 `dot` Scalar x2 = Scalar $ x1 * x2
Tensor (Covariant count1 _) ts1 `dot` Tensor (Contravariant count2 _) ts2
    | count1 == count2 = ts1 `Linear.Metric.dot` ts2
    | otherwise = error $ "dot: " P.++ incompatibleTypes
_ `dot` _ = error $ "dot: " P.++ incompatibleTypes


-- Tensor can be trated as a number with all its operations
instance Num a => Num (Tensor a) where
    -- Adding
    Scalar x1 + Scalar x2 = Scalar (x1 + x2)
    Tensor index1 v1 + Tensor index2 v2
        | index1 == index2 = Tensor index1 (v1 ^+^ v2)
        | otherwise = error $ "add: " P.++ incompatibleTypes
    Scalar x + t = (x+) <$> t
    t + Scalar x = (+x) <$> t

    -- Subtracting
    Scalar x1 - Scalar x2 = Scalar (x1 - x2)
    Tensor index1 v1 - Tensor index2 v2
        | index1 == index2 = Tensor index1 (v1 ^-^ v2)
        | otherwise = error $ "subtract: " P.++ incompatibleTypes
    Scalar x - t = (\e -> x - e) <$> t
    t - Scalar x = (\e -> e - x) <$> t

    -- Multiplication - simple multiplication, elem by elem
    Scalar x1 * Scalar x2 = Scalar (x1 * x2)
    (Tensor index1 v1) * (Tensor index2 v2)
        | index1 == index2 = Tensor index1 (I.zipWith (*) v1 v2)
        | otherwise = error $ "multiplicate: " P.++ incompatibleTypes
    Scalar x * t = (x*) <$> t
    t * Scalar x = (*x) <$> t

    -- Absolute value - elem by elem
    abs (Scalar x) = Scalar $ abs x
    abs (Tensor index ts) = Tensor index (abs <$> ts)

    -- Signum operation - elem by elem
    signum (Scalar x) = Scalar $ signum x
    signum (Tensor index ts) = Tensor index (signum <$> ts)

    -- Simple integer can be oonveted to Scalar
    fromInteger x = Scalar $ fromInteger x
-}



