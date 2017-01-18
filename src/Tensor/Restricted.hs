-----------------------------------------------------------------------------
--
-- Package     :  Tensor
-- Module      :  Tensor.Restricted
-- Description :  Implements a restricted tensor i.e. tensor with rigidly defined size
-- Author      :  Artur M. Brodzki, Warsaw 2016
-----------------------------------------------------------------------------

{-# LANGUAGE Strict #-}
{-# OPTIONS_GHC #-}

module Tensor.Restricted (
    Tensor(..),
    kr, kr3, kr4, switchInd, switchInd', commonIndex, _dot
) where

import           Control.Monad
import           Data.List          as L
import           Data.Maybe
import           Data.Vector        as V
import           Data.Vector.Binary
import           Prelude            as P
import           Tensor.Index
import           Tensor.Multilinear as T
--import Control.Lens
import           Data.Binary

{- ERROR MESSAGES -}
indexOutOfRange :: String
indexOutOfRange = "Index out of range!"
zeroElems :: String
zeroElems = "Tensor must contain at leat one element!"
incompatibleTypes :: String
incompatibleTypes = "Incompatible tensor types!"
noIndex :: String
noIndex = "No such index in this tensor!"

{- RESTRICTED TENSOR DATATYPE -}

data Tensor a =
    Scalar { scalarVal :: a } |
    Tensor {
        tensorIndex :: TIndex,
        tensorData  :: Vector (Tensor a)
    }
    deriving Eq

--makeLenses ''Tensor

instance Binary a => Binary (Tensor a) where
    put (Scalar x) = do
        put (0 :: Word8)
        put x

    put (Tensor ind ts) = do
        put (1 :: Word8)
        put ind
        genericPutVector ts

    get = do
        f <- get :: Get Word8
        if f == 0
        then do
            x <- get
            return $ Scalar x
        else do
            ind <- get
            ts <- genericGetVector
            return $ Tensor ind ts


_showVectorVertical :: Show a => V.Vector a -> String
_showVectorVertical v =
    "\n  " P.++ ((P.++) `V.foldl1` vShowed)
        where vShowed = (\x -> "| " P.++ show x P.++ "\n  ") <$> v :: V.Vector String

_alignTo :: String -> Int -> String
_alignTo s n = L.replicate (n - L.length s) ' ' P.++ s

-- Print tensor
instance Show a => Show (Tensor a) where
    show (Scalar x) = show x
    show (Tensor index@(Covariant _ _) ts) =
        show index P.++ " " P.++ show ts
    show (Tensor index@(Contravariant _ _) ts)=
        show index P.++ " " P.++ _showVectorVertical ts
    show (Tensor index@(Indifferent _ _) ts) =
        show index P.++ " " P.++ show ts

-- Tensor is a Functor
instance Functor Tensor where
    fmap f (Scalar v_) = Scalar (f v_)
    fmap f (Tensor indexT ts) = Tensor indexT v2
        where v2 = fmap (fmap f) ts

{-
instance Applicative Tensor where
    pure = Scalar

    Scalar f <*> t = f <$> t
    Tensor ind fs <*> t = T.generate ind (\i -> (fs ! i) <$> t)
    -}

-- Tensor can be treated as a number with all its operations
instance Num a => Num (Tensor a) where
    -- Adding
    Scalar x1 + Scalar x2 = Scalar $ x1 + x2
    Scalar x + t = (x+) <$> t
    t + Scalar x = (+x) <$> t
    Tensor index1 v1 + Tensor index2 v2
        | index1 Tensor.Index.!=! index2
            = Tensor index1 $ V.zipWith (+) v1 v2
        | otherwise = error $
                "add: " P.++ incompatibleTypes P.++
                " - index1 is " P.++ show index1 P.++
                " and index2 is " P.++ show index2 P.++ "!"

    -- Subtracting
    Scalar x1 - Scalar x2 = Scalar $ x1 - x2
    Scalar x - t = (\e -> x - e) <$> t
    t - Scalar x = (\e -> e - x) <$> t
    Tensor index1 v1 - Tensor index2 v2
        | index1 Tensor.Index.!=! index2 =
            Tensor index1 $ V.zipWith (-) v1 v2
        | otherwise = error $
            "subtract: " P.++ incompatibleTypes P.++
            " - index1 is " P.++ show index1 P.++
            " and index2 is " P.++ show index2

    Scalar x1 * Scalar x2 = Scalar $ x1 * x2
    Scalar x1 * t = (x1*) <$> t
    t * Scalar x2 = (*x2) <$> t
    t1 * t2 = t1 !* t2

    -- Absolute value - elem by elem
    abs (Scalar x)        = Scalar $ abs x
    abs (Tensor index ts) = Tensor index (abs <$> ts)

    -- Signum operation - elem by elem
    signum (Scalar x)        = Scalar $ signum x
    signum (Tensor index ts) = Tensor index (signum <$> ts)

    -- Simple integer can be oonveted to Scalar
    fromInteger x = Scalar $ fromInteger x

-- Tensors can be divided by each other
instance Fractional a => Fractional (Tensor a) where
    Scalar x1 / Scalar x2 = Scalar $ x1 / x2
    Scalar x1 / t2 = (x1 /) <$> t2
    t1 / Scalar x2 = (/ x2) <$> t1
    t1@(Tensor index1 ts1) / t2@(Tensor index2 ts2)
        | index1 Tensor.Index.!=! index2 = Tensor index1 $ V.zipWith (/) ts1 ts2
        | otherwise = error $
            "(/): " P.++ incompatibleTypes P.++
            " tensor1 has type " P.++ show (indices t1) P.++
            " and tensor2 has type " P.++ show (indices t2)

    fromRational x = Scalar $ fromRational x

-- Real-number functions on tensors.
-- Function of tensor is a tensor of function of its elements
-- E.g. exp [1,2,3,4] = [exp 1, exp2, exp3, exp4]
instance Floating a => Floating (Tensor a) where
    pi = Scalar pi

    exp (Scalar x)        = Scalar $ exp x
    exp (Tensor index ts) = Tensor index (exp <$> ts)

    log (Scalar x)        = Scalar $ log x
    log (Tensor index ts) = Tensor index (log <$> ts)

    sin (Scalar x)        = Scalar $ sin x
    sin (Tensor index ts) = Tensor index (sin <$> ts)

    cos (Scalar x)        = Scalar $ cos x
    cos (Tensor index ts) = Tensor index (cos <$> ts)

    asin (Scalar x)        = Scalar $ asin x
    asin (Tensor index ts) = Tensor index (asin <$> ts)

    acos (Scalar x)        = Scalar $ acos x
    acos (Tensor index ts) = Tensor index (acos <$> ts)

    atan (Scalar x)        = Scalar $ atan x
    atan (Tensor index ts) = Tensor index (atan <$> ts)

    sinh (Scalar x)        = Scalar $ sinh x
    sinh (Tensor index ts) = Tensor index (sinh <$> ts)

    cosh (Scalar x)        = Scalar $ cosh x
    cosh (Tensor index ts) = Tensor index (cosh <$> ts)

    asinh (Scalar x)        = Scalar $ asinh x
    asinh (Tensor index ts) = Tensor index (asinh <$> ts)

    acosh (Scalar x)        = Scalar $ acosh x
    acosh (Tensor index ts) = Tensor index (acosh <$> ts)

    atanh (Scalar x)        = Scalar $ atanh x
    atanh (Tensor index ts) = Tensor index (atanh <$> ts)

-- Tensor operations
instance Multilinear Tensor where
    -- Safe indexing
    Scalar _ !? _ = error indexOutOfRange
    Tensor _ ts1 !? ind =
        if isNothing (ts1 V.!? ind)
        then error  $ "(!?): " P.++ indexOutOfRange
        else Just $ ts1 V.! ind
{-
    -- Projection
    Scalar x !! [] = Just $ Scalar x
    Scalar _ !! _  = Nothing
    (Tensor ind ts) !! [] = Just $ Tensor ind ts
    t@(Tensor ind ts) !! is =
        let indexVal = indexName ind `lookup` is
        in
            if isJust indexVal
            then (t T.!? fromJust indexVal) T.!! (_deleteAll (indexName ind) is)
            else Tensor ind $ (T.!! is) <$> ts -}

    -- Tensor product with Einstein summation convention
    (Scalar x1) !* (Scalar x2) = Scalar $ x1 * x2
    (Scalar x) !* t2 = (x P.*) <$> t2
    t1 !* (Scalar x) = (P.* x) <$> t1
    t1 !* t2 = switchInd' t1 (commonIndex t1 t2) !*! switchInd' t2 (commonIndex t1 t2)

    -- Generate tensor from generator function of indices
    generate index f
        | indexCount index > 0 = Tensor index $ V.generate (indexCount index) f
        | otherwise = error $ "generate: " P.++ zeroElems

    -- Get tensor order [ (p,q)-type ]
    order (Scalar _) = (0,0)
    order (Tensor (Contravariant _ _) t) = (cnvr+1,covr)
        where (cnvr,covr) = order $ V.head t
    order (Tensor (Covariant _ _) t) = (cnvr,covr+1)
        where (cnvr,covr) = order $ V.head t
    order (Tensor (Indifferent _ _) t) = (cnvr,covr)
        where (cnvr,covr) = order $ V.head t

    -- Get number of elems in tensor
    elems (Scalar _)        = 1
    elems (Tensor index ts) = indexCount index P.* elems (V.head ts)

    -- Get list of all tensor indices
    indices (Scalar _)        = []
    indices (Tensor index ts) = index : indices (V.head ts)

    -- Check if two tensors are equivalent (so are the same type and size)
    equiv t1 t2 = L.and $ P.zipWith (Tensor.Index.!=!) (indices t1) (indices t2)

    -- Rename tensor index
    rename (Scalar x) _ _ = Scalar x
    rename t@(Tensor (Contravariant count name) ts) from to
        | name == from = Tensor (Contravariant count to) $ (\t' -> rename t' from to) <$> ts
        | otherwise = t
    rename t@(Tensor (Covariant count name) ts) from to
        | name == from = Tensor (Covariant count to) $ (\t' -> rename t' from to) <$> ts
        | otherwise = t
    rename t@(Tensor (Indifferent count name) ts) from to
        | name == from = Tensor (Indifferent count to) $ (\t' -> rename t' from to) <$> ts
        | otherwise = t

    -- Transpose a tensor (switch all indices types)
    transpose (Scalar x) = Scalar x
    transpose (Tensor (Covariant count name) ts) =
        Tensor (Contravariant count name) (T.transpose <$> ts)
    transpose (Tensor (Contravariant count name) ts) =
        Tensor (Covariant count name) (T.transpose <$> ts)
    transpose (Tensor (Indifferent count name) ts) =
        Tensor (Indifferent count name) (T.transpose <$> ts)

    -- Transpose a tensor - switch only the first index
    transpose1 (Scalar x) = Scalar x
    transpose1 (Tensor (Covariant count name) ts) =
        Tensor (Contravariant count name) ts
    transpose1 (Tensor (Contravariant count name) ts) =
        Tensor (Covariant count name) ts
    transpose1 (Tensor (Indifferent count name) ts) =
        Tensor (Indifferent count name) ts

    -- Concatenation of tensor by given index or with creating a new one
    concat (Covariant _ name) s1@(Scalar _) s2@(Scalar _) =
        Tensor (Covariant 2 name) $ V.fromList [s1,s2]
    concat (Contravariant _ name) s1@(Scalar _) s2@(Scalar _) =
        Tensor (Contravariant 2 name) $ V.fromList [s1,s2]
    concat index@(Covariant _ name) (Tensor index1 ts1) (Tensor index2 ts2)
        | index1 == index && index2 == index =
            Tensor (Covariant (indexCount index1 + indexCount index2) name) (ts1 V.++ ts2)
        | index1 Tensor.Index.!=! index2 =
            Tensor index1 $ V.generate (indexCount index1) (\i -> T.concat index (ts1 V.! i) (ts2 V.! i))
    concat index@(Contravariant _ name) (Tensor index1 ts1) (Tensor index2 ts2)
        | index1 == index && index2 == index =
            Tensor (Contravariant (indexCount index1 + indexCount index2) name) (ts1 V.++ ts2)
        | index1 Tensor.Index.!=! index2 =
            Tensor index1 $ V.generate (indexCount index1) (\i -> T.concat index (ts1 V.! i) (ts2 V.! i))
    concat _ t1 t2 = error $
        "concat: " P.++ incompatibleTypes P.++
        " tensor1 has type " P.++ show (indices t1) P.++
        " and tensor2 has type " P.++ show (indices t2)

    -- Generate 1-rank tensor from a list
    fromList (Contravariant _ name) ls =
        Tensor (Contravariant (P.length ls) name) $ V.fromList $ Scalar <$> ls
    fromList (Covariant _ name) ls =
        Tensor (Covariant (P.length ls) name) $ V.fromList $ Scalar <$> ls
    fromList (Indifferent _ name) ls =
        Tensor (Indifferent (P.length ls) name) $ V.fromList $ Scalar <$> ls

-- Push index given as Maybe String one step deeper in recursion
switchInd :: Tensor a -> Maybe String -> Tensor a
switchInd (Scalar x) _ = Scalar x
switchInd t1 Nothing = t1
switchInd t1@(Tensor index1 ts1) (Just ind)
    | P.length (indices t1) > 1 && indexName index1 == ind =
        let index2 = tensorIndex (ts1 V.! 0)
        in Tensor index2 $ V.generate (indexCount index2)
            (\i -> Tensor index1 $ V.generate (indexCount index1)
                (\j -> tensorData (ts1 V.! j) V.! i))
    | otherwise = t1

-- Push index given as Maybe String into the deepest level of recursion
switchInd' :: Tensor a -> Maybe String -> Tensor a
switchInd' (Scalar x) _ = Scalar x
switchInd' t1 Nothing = t1
switchInd' t1@(Tensor index1 _) i@(Just ind)
    | P.length (indices t1) > 1 && indexName index1 == ind =
        let t2 = switchInd t1 i
        in Tensor (tensorIndex t2) $ (`switchInd'` i) <$> tensorData t2
    | otherwise = t1

-- Dot product of covector and vector (specifically in this order)
-- Number of elements in two tensors must be the same
_dot :: Num a => Tensor a -> Tensor a -> Tensor a
Scalar x1 `_dot` Scalar x2 =  Scalar $ x1 P.* x2
Tensor i1@(Covariant count1 _) ts1 `_dot` Tensor i2@(Contravariant count2 _) ts2
    | count1 == count2 = V.sum $ V.zipWith (*) ts1 ts2
    | otherwise = error $
        "Tensor product: " P.++ incompatibleTypes P.++
        " - index1 is " P.++ show i1 P.++
        " and index2 is " P.++ show i2
t1 `_dot` t2 = error $
    "Tensor product: " P.++ incompatibleTypes P.++
    " - index1 is " P.++ show (tensorIndex t1) P.++
    " and index2 is " P.++ show (tensorIndex t2)

-- Kronecker delta with rank 2
kr :: Num a => Int -> Tensor a
kr size =
    T.generate (Covariant size "i") (\i ->
        T.generate (Covariant size "j") (\j ->
            Scalar $ if i == j then 1 else 0 ))

-- Kronecker delta with rank 3
kr3 :: Num a => Int -> Tensor a
kr3 size =
    T.generate (Covariant size "i") (\i ->
        T.generate (Covariant size "j") (\j ->
            T.generate (Covariant size "k") (\k ->
                Scalar $ if i == j && j == k then 1 else 0 )))

-- Kronecker delta with rank 4
kr4 :: Num a => Int -> Tensor a
kr4 size =
    T.generate (Covariant size "i") (\i ->
        T.generate (Covariant size "j") (\j ->
            T.generate (Covariant size "k") (\k ->
                T.generate (Covariant size "l") (\l ->
                    Scalar $ if i == j && j == k && k == l then 1 else 0 ))))


-- Tensor product with Einstein summation convention
(!*!) :: Num a => Tensor a -> Tensor a -> Tensor a
(Scalar x1) !*! (Scalar x2) = Scalar $ x1 * x2
(Scalar x) !*! t2 = (x P.*) <$> t2
t1 !*! (Scalar x) = (P.* x) <$> t1
t1@(Tensor index1 ts1) !*! t2@(Tensor index2 ts2)
    | indexName index1 == indexName index2 = t1 `Tensor.Restricted._dot` t2
    | indexName index1 `P.elem` (indexName <$> indices t2) =
        T.generate index2 (\i -> t1 !*! (ts2 V.! i))
    | otherwise = T.generate index1 (\i -> (ts1 V.! i) !*! t2)

-- Find common index in two tensors, if any
commonIndex :: Tensor a -> Tensor a -> Maybe String
commonIndex (Scalar _) _ = Nothing
commonIndex _ (Scalar _) = Nothing
commonIndex t1@(Tensor _ _) t2@(Tensor _ _) =
    let indicesNames1 = indexName <$> indices t1
        indicesNames2 = indexName <$> indices t2
    in msum $ (\i -> L.find (==i) indicesNames2) <$> indicesNames1
