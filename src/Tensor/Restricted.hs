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

{- RESTRICTED TENSOR DATATYPE -}

data Tensor a =
    Scalar { scalarVal :: a } |
    Tensor {
        tensorIndex :: TIndex,
        tensorData  :: [Tensor a]
    } | 
    Err { errMsg :: String }
    deriving Eq

--makeLenses ''Tensor

-- Tensor serialization and deserialization
instance Binary a => Binary (Tensor a) where
    put (Scalar x) = do
        put (0 :: Word8)
        put x

    put (Tensor ind ts) = do
        put (1 :: Word8)
        put ind
        put ts
    
    put (Err msg) = do
        put (2 :: Word8)
        put msg

    get = do
        f <- get :: Get Word8
        case f of
            0 -> do
                x <- get
                return $ Scalar x
            1 -> do
                ind <- get
                ts <- get
                return $ Tensor ind ts
            2 -> do
                msg <- get
                return $ Err msg
            _ -> return $ Err "Cannot deserialize tensor!"


_showVertical :: Show a => [a] -> String
_showVertical v =
    "\n  " P.++ ((P.++) `foldl1` vShowed)
        where vShowed = (\x -> "| " P.++ show x P.++ "\n  ") <$> v :: [String]

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
    show (Err msg) = show msg

-- Tensor is a Functor
instance Functor Tensor where
    fmap f (Scalar v_) = Scalar (f v_)
    fmap f (Tensor indexT ts) = Tensor indexT v2
        where v2 = fmap (fmap f) ts
    fmap _ (Err msg) = Err msg

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
    Tensor index1 v1 + Tensor index2 v2 =
        if index1 !=! index2
        then Tensor index1 $ zipWith (+) v1 v2
        else Err $ 
            "add: " P.++ incompatibleTypes P.++ 
            " - index1 is " P.++ show index1 P.++ 
            " and index2 is " P.++ show index2
    Err msg + _ = Err msg
    _ + Err msg = Err msg

    -- Subtracting
    Scalar x1 - Scalar x2 = Scalar $ x1 - x2
    Scalar x - t = (\e -> x - e) <$> t
    t - Scalar x = (\e -> e - x) <$> t
    Tensor index1 v1 - Tensor index2 v2 = 
        if index1 !=! index2
        then Tensor index1 $ zipWith (-) v1 v2
        else Err $
            "subtract: " P.++ incompatibleTypes P.++
            " - index1 is " P.++ show index1 P.++
            " and index2 is " P.++ show index2
    Err msg - _ = Err msg
    _ - Err msg = Err msg

    Scalar x1 * Scalar x2 = Scalar $ x1 * x2
    Scalar x1 * t = (x1*) <$> t
    t * Scalar x2 = (*x2) <$> t
    t1 * t2 = t1 !* t2

    -- Absolute value - elem by elem
    abs (Scalar x)        = Scalar $ abs x
    abs (Tensor index ts) = Tensor index (abs <$> ts)
    abs (Err msg) = Err msg

    -- Signum operation - elem by elem
    signum (Scalar x)        = Scalar $ signum x
    signum (Tensor index ts) = Tensor index (signum <$> ts)
    signum (Err msg) = Err msg

    -- Simple integer can be oonveted to Scalar
    fromInteger x = Scalar $ fromInteger x

-- Tensors can be divided by each other
instance Fractional a => Fractional (Tensor a) where
    Scalar x1 / Scalar x2 = Scalar $ x1 / x2
    Scalar x1 / t2 = (x1 /) <$> t2
    t1 / Scalar x2 = (/ x2) <$> t1
    t1@(Tensor index1 ts1) / t2@(Tensor index2 ts2) = 
        if index1 !=! index2
        then Tensor index1 $ zipWith (/) ts1 ts2
        else Err $
            "(/): " P.++ incompatibleTypes P.++
            " tensor1 has type " P.++ show (indices t1) P.++
            " and tensor2 has type " P.++ show (indices t2)
    Err msg / _ = Err msg
    _ / Err msg = Err msg

    fromRational x = Scalar $ fromRational x

-- Real-number functions on tensors.
-- Function of tensor is a tensor of function of its elements
-- E.g. exp [1,2,3,4] = [exp 1, exp2, exp3, exp4]
instance Floating a => Floating (Tensor a) where
    pi = Scalar pi

    exp (Scalar x)        = Scalar $ exp x
    exp (Tensor index ts) = Tensor index (exp <$> ts)
    exp (Err msg) = Err msg

    log (Scalar x)        = Scalar $ log x
    log (Tensor index ts) = Tensor index (log <$> ts)
    log (Err msg) = Err msg

    sin (Scalar x)        = Scalar $ sin x
    sin (Tensor index ts) = Tensor index (sin <$> ts)
    sin (Err msg) = Err msg

    cos (Scalar x)        = Scalar $ cos x
    cos (Tensor index ts) = Tensor index (cos <$> ts)
    cos (Err msg) = Err msg

    asin (Scalar x)        = Scalar $ asin x
    asin (Tensor index ts) = Tensor index (asin <$> ts)
    asin (Err msg) = Err msg

    acos (Scalar x)        = Scalar $ acos x
    acos (Tensor index ts) = Tensor index (acos <$> ts)
    acos (Err msg) = Err msg

    atan (Scalar x)        = Scalar $ atan x
    atan (Tensor index ts) = Tensor index (atan <$> ts)
    atan (Err msg) = Err msg

    sinh (Scalar x)        = Scalar $ sinh x
    sinh (Tensor index ts) = Tensor index (sinh <$> ts)
    sinh (Err msg) = Err msg

    cosh (Scalar x)        = Scalar $ cosh x
    cosh (Tensor index ts) = Tensor index (cosh <$> ts)
    cosh (Err msg) = Err msg

    asinh (Scalar x)        = Scalar $ asinh x
    asinh (Tensor index ts) = Tensor index (asinh <$> ts)
    asinh (Err msg) = Err msg

    acosh (Scalar x)        = Scalar $ acosh x
    acosh (Tensor index ts) = Tensor index (acosh <$> ts)
    acosh (Err msg) = Err msg

    atanh (Scalar x)        = Scalar $ atanh x
    atanh (Tensor index ts) = Tensor index (atanh <$> ts)
    atanh (Err msg) = Err msg

-- Tensor operations
instance Multilinear Tensor where
    -- Safe indexing
    Scalar _ !? _ = Err $ "(!?): " P.++ indexOutOfRange
    Tensor _ ts1 !? ind =
        if isNothing (ts1 !? ind)
        then Err  $ "(!?): " P.++ indexOutOfRange
        else ts1 ! ind
    Err msg !? _ = Err msg
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
    t1@(Tensor _ _) !* t2@(Tensor _ _) = 
        let cmi = commonIndex t1 t2
        in switchInd' t1 cmi !*! switchInd' t2 cmi
    Err msg !* _ = Err msg
    _ !* Err msg = Err msg
    
    -- Generate tensor from generator function of indices
    generate index f = 
        if indexCount index > 0
        then Tensor index [f i | i <- [1 .. indexCount index]]
        else Err $ "generate: " P.++ zeroElems

    -- Get tensor order [ (p,q)-type ]
    order (Scalar _) = (0,0)
    order (Tensor (Contravariant _ _) t) = (cnvr+1,covr)
        where (cnvr,covr) = order $ head t
    order (Tensor (Covariant _ _) t) = (cnvr,covr+1)
        where (cnvr,covr) = order $ head t
    order (Tensor (Indifferent _ _) t) = (cnvr,covr)
        where (cnvr,covr) = order $ head t

    -- Get number of elems in tensor
    elems (Scalar _)        = 1
    elems (Tensor index ts) = indexCount index P.* elems (head ts)

    -- Get list of all tensor indices
    indices (Scalar _)        = []
    indices (Tensor index ts) = index : indices (head ts)

    -- Check if two tensors are equivalent (so are the same type and size)
    equiv t1 t2 = and $ zipWith (!=!) (indices t1) (indices t2)

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

    -- Generate 1-rank tensor from a list
    fromList (Contravariant _ name) ls =
        Tensor (Contravariant (P.length ls) name) $ Scalar <$> ls
    fromList (Covariant _ name) ls =
        Tensor (Covariant (P.length ls) name) $ Scalar <$> ls
    fromList (Indifferent _ name) ls =
        Tensor (Indifferent (P.length ls) name) $ Scalar <$> ls

-- Push index given as Maybe String one step deeper in recursion
switchInd :: Tensor a -> Maybe String -> Tensor a
switchInd (Scalar x) _ = Scalar x
switchInd t1 Nothing = t1
switchInd t1@(Tensor index1 ts1) (Just ind)
    | P.length (indices t1) > 1 && indexName index1 == ind =
        let index2 = tensorIndex (ts1 ! 0)
        in Tensor index2 [Tensor index1 [tensorData (ts1 ! j) ! i 
            | j <- [1 .. indexCount index1]] 
            | i <- [1 .. indexCount index2]]
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
    | count1 == count2 = sum $ zipWith (*) ts1 ts2
    | otherwise = Err $
        "Tensor product: " P.++ incompatibleTypes P.++
        " - index1 is " P.++ show i1 P.++
        " and index2 is " P.++ show i2
t1 `_dot` t2 = Err $
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
        T.generate index2 (\i -> t1 !*! (ts2 ! i))
    | otherwise = T.generate index1 (\i -> (ts1 ! i) !*! t2)

-- Find common index in two tensors, if any
commonIndex :: Tensor a -> Tensor a -> Maybe String
commonIndex (Scalar _) _ = Nothing
commonIndex _ (Scalar _) = Nothing
commonIndex t1@(Tensor _ _) t2@(Tensor _ _) =
    let indicesNames1 = indexName <$> indices t1
        indicesNames2 = indexName <$> indices t2
    in msum $ (\i -> L.find (==i) indicesNames2) <$> indicesNames1



