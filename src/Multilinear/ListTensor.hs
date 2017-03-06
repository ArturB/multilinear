{-|
Module      : Multilinear
Description : Provides efficient and generic implementation of linear algebra operation using Ricci - Einstein tensor formalism
Copyright   : (c) Artur M. Brodzki, 2017
License     : 3-clause BSD
Maintainer  : artur.brodzki@gmail.com
Stability   : experimental
Portability : Windows/POSIX

-}

{-# LANGUAGE Strict, GADTs #-}
{-# OPTIONS_GHC #-}

module Multilinear.ListTensor (
    Tensor(..),
    switchInd, switchInd', commonIndex, _dot
) where

import           Multilinear
import           Control.Monad
import           Data.List
import           Data.Maybe
import           Data.Binary

{- ERROR MESSAGES -}
incompatibleTypes :: String
incompatibleTypes = "Incompatible tensor types!"
deserializationErr :: String
deserializationErr = "Tensor deserialization error!"

{- Tensor defined recursively as scalar or list of other tensors -}
data Tensor a =
    {-| Tensor may be a scalar -}
    Scalar { 
        scalarVal :: a 
    } |
    {-| Or a list of other tensors -}
    Tensor { 
        tensorIndex :: TIndex, 
        tensorData :: [Tensor a] 
    } |
    {-| Operations on tensors may throw an error -}
    Err { 
        errMessage :: String 
    } deriving Eq

{-| Tensor serialization and deserialization -}
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
            _ -> return $ Err deserializationErr

-- print list elements vertically
-- used to show contravariant components of tensor, which by convention are printed vertically
_showVertical :: Show a => [a] -> String
_showVertical v =
    "\n  " ++ ((++) `foldl1` vShowed)
        where vShowed = (\x -> "| " ++ show x ++ "\n  ") <$> v :: [String]

-- return True if tensor is an error
_isErrTensor :: Tensor a -> Bool
_isErrTensor (Err _) = True
_isErrTensor _       = False

-- collapse many errors in tensor to the first one
_collapseErr :: Tensor a -> Tensor a
_collapseErr t1@(Tensor _ ts) =
    -- find first error if present
    let err = Data.List.find _isErrTensor (_collapseErr <$> ts) 
    -- and return this error if found, whole tensor otherwise
    in fromMaybe t1 err 
-- in scalars cannot be any error
_collapseErr t = t

-- Show tensor without collapsing
show' :: Show a => Tensor a -> String
-- Scalar is showed simply as its value
show' (Scalar x) = show x
-- Covariant components are shown horizontally
show' (Tensor index@(Covariant _ _) ts) =
    show index ++ " " ++ show ts
-- Contravariant components are shown vertically
show' (Tensor index@(Contravariant _ _) ts)=
    show index ++ " " ++ _showVertical ts
-- Sequences are shown horizontally
show' (Tensor index@(Indifferent _ _) ts) =
    show index ++ " " ++ show ts
-- Error prints it error message
show' (Err msg) = show msg

-- Print tensor
instance Show a => Show (Tensor a) where
    -- collapse errors first and then print tensor
    show t = show' $ _collapseErr t

-- Tensor is a Functor
instance Functor Tensor where
    -- Mapping scalars simply maps its value
    fmap f (Scalar v_) = Scalar (f v_)
    -- Mapping tensors does mapping element by element
    fmap f (Tensor indexT ts) = Tensor indexT v2
        where v2 = fmap (fmap f) ts
    -- Mapping errors changes nothing
    fmap _ (Err msg) = Err msg

-- Tensor can be added, subtracted and multiplicated
instance Num a => Num (Tensor a) where
    -- Adding - element by element
    Scalar x1 + Scalar x2 = Scalar $ x1 + x2
    Scalar x + t = (x+) <$> t
    t + Scalar x = (+x) <$> t
    Tensor index1 v1 + Tensor index2 v2 =
        if index1 == index2
        then Tensor index1 $ zipWith (+) v1 v2
        else Err $
            "add: " ++ incompatibleTypes ++
            " - index1 is " ++ show index1 ++
            " and index2 is " ++ show index2
    Err msg + _ = Err msg
    _ + Err msg = Err msg

    -- Subtracting - element by element
    Scalar x1 - Scalar x2 = Scalar $ x1 - x2
    Scalar x - t = (\e -> x - e) <$> t
    t - Scalar x = (\e -> e - x) <$> t
    Tensor index1 v1 - Tensor index2 v2 =
        if index1 !=! index2
        then Tensor index1 $ zipWith (-) v1 v2
        else Err $
            "subtract: " ++ incompatibleTypes ++
            " - index1 is " ++ show index1 ++
            " and index2 is " ++ show index2
    Err msg - _ = Err msg
    _ - Err msg = Err msg

    -- Multiplicating is treated as tensor product
    Scalar x1 * Scalar x2 = Scalar $ x1 * x2
    Scalar x1 * t = (x1*) <$> t
    t * Scalar x2 = (*x2) <$> t
    t1 * t2 = t1 !* t2

    -- Absolute value - element by element
    abs (Scalar x)        = Scalar $ abs x
    abs (Tensor index ts) = Tensor index (abs <$> ts)
    abs (Err msg)         = Err msg

    -- Signum operation - element by element
    signum (Scalar x)        = Scalar $ signum x
    signum (Tensor index ts) = Tensor index (signum <$> ts)
    signum (Err msg)         = Err msg

    -- Simple integer can be conveted to Scalar
    fromInteger x = Scalar $ fromInteger x

-- Tensors can be divided by each other
instance Fractional a => Fractional (Tensor a) where
    -- Scalar division return result of division of its values
    Scalar x1 / Scalar x2 = Scalar $ x1 / x2
    -- Tensor and scalar are divided value by value
    Scalar x1 / t2 = (x1 /) <$> t2
    t1 / Scalar x2 = (/ x2) <$> t1
    -- Two tensors are divided by corresponding elements
    t1@(Tensor index1 ts1) / t2@(Tensor index2 ts2) =
        if index1 == index2
        then Tensor index1 $ zipWith (/) ts1 ts2
        else Err $
            "(/): " ++ incompatibleTypes ++
            " tensor1 has type " ++ show (indices t1) ++
            " and tensor2 has type " ++ show (indices t2)
    Err msg / _ = Err msg
    _ / Err msg = Err msg

    -- Scalar can be generated from rational number
    fromRational x = Scalar $ fromRational x

-- Real-number functions on tensors.
-- Function of tensor is a tensor of function of its elements
-- E.g. exp [1,2,3,4] = [exp 1, exp2, exp3, exp4]
instance Floating a => Floating (Tensor a) where
    pi = Scalar pi

    exp (Scalar x)        = Scalar $ exp x
    exp (Tensor index ts) = Tensor index (exp <$> ts)
    exp (Err msg)         = Err msg

    log (Scalar x)        = Scalar $ log x
    log (Tensor index ts) = Tensor index (log <$> ts)
    log (Err msg)         = Err msg

    sin (Scalar x)        = Scalar $ sin x
    sin (Tensor index ts) = Tensor index (sin <$> ts)
    sin (Err msg)         = Err msg

    cos (Scalar x)        = Scalar $ cos x
    cos (Tensor index ts) = Tensor index (cos <$> ts)
    cos (Err msg)         = Err msg

    asin (Scalar x)        = Scalar $ asin x
    asin (Tensor index ts) = Tensor index (asin <$> ts)
    asin (Err msg)         = Err msg

    acos (Scalar x)        = Scalar $ acos x
    acos (Tensor index ts) = Tensor index (acos <$> ts)
    acos (Err msg)         = Err msg

    atan (Scalar x)        = Scalar $ atan x
    atan (Tensor index ts) = Tensor index (atan <$> ts)
    atan (Err msg)         = Err msg

    sinh (Scalar x)        = Scalar $ sinh x
    sinh (Tensor index ts) = Tensor index (sinh <$> ts)
    sinh (Err msg)         = Err msg

    cosh (Scalar x)        = Scalar $ cosh x
    cosh (Tensor index ts) = Tensor index (cosh <$> ts)
    cosh (Err msg)         = Err msg

    asinh (Scalar x)        = Scalar $ asinh x
    asinh (Tensor index ts) = Tensor index (asinh <$> ts)
    asinh (Err msg)         = Err msg

    acosh (Scalar x)        = Scalar $ acosh x
    acosh (Tensor index ts) = Tensor index (acosh <$> ts)
    acosh (Err msg)         = Err msg

    atanh (Scalar x)        = Scalar $ atanh x
    atanh (Tensor index ts) = Tensor index (atanh <$> ts)
    atanh (Err msg)         = Err msg


-- Multilinear operations
instance Multilinear Tensor where
    -- Add scalar left
    x .+ t = (x+) <$> t

    -- Subtract scalar left
    x .- t = (x-) <$> t

    -- Multiplicate by scalar left
    x .* t = (x*) <$> t

    -- Divide by scalar left
    x ./ t = (x/) <$> t

    -- Add scalar right
    t +. x = (+x) <$> t

    -- Subtract scalar right
    t -. x = (\p -> p - x) <$> t

    -- Multiplicate by scalar right
    t *. x = (*x) <$> t

    -- Divide by scalar right
    t /. x = (/x) <$> t

    -- Get tensor order [ (p,q)-type ]
    order (Scalar _) = (0,0)
    order (Tensor (Contravariant _ _) t) = (cnvr+1,covr)
        where (cnvr,covr) = order $ head t
    order (Tensor (Covariant _ _) t) = (cnvr,covr+1)
        where (cnvr,covr) = order $ head t
    order (Tensor (Indifferent _ _) t) = (cnvr,covr)
        where (cnvr,covr) = order $ head t
    order (Err _) = (-1,-1)

    -- Get number of elements in tensor
    elems (Scalar _)        = 1
    elems (Tensor index ts) = indexCount index * elems (head ts)
    elems (Err _)           = -1

    -- Get list of all tensor indices
    indices (Scalar _)        = []
    indices (Tensor index ts) = index : indices (head ts)
    indices (Err _)           = []

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
    rename (Err msg) _ _ = Err msg

    -- Transpose a tensor (switch all indices types)
    transpose (Scalar x) = Scalar x
    transpose (Tensor (Covariant count name) ts) =
        Tensor (Contravariant count name) (Multilinear.transpose <$> ts)
    transpose (Tensor (Contravariant count name) ts) =
        Tensor (Covariant count name) (Multilinear.transpose <$> ts)
    transpose (Tensor (Indifferent count name) ts) =
        Tensor (Indifferent count name) (Multilinear.transpose <$> ts)
    transpose (Err msg) = Err msg

    -- Inverse a tensor as a multilinear map
    -- TODO
    inverse t = t

    {-| Concatenation of two tensor with given index or by creating a new one -}
    -- TODO
    concat _ _ t = t

-- Push index given as Maybe String one step deeper in recursion
-- Important for performance - tensor product is most efficient if summed indices are on the deepest level of recursion
switchInd :: Num a => Tensor a -> Maybe String -> Tensor a
switchInd (Scalar x) _ = Scalar x
switchInd t1 Nothing = t1
switchInd t1@(Tensor index1 ts1) (Just ind)
    | length (indices t1) > 1 && indexName index1 == ind =
        let index2 = tensorIndex (head ts1)
        in Tensor index2 [Tensor index1 [tensorData (ts1 !! j) !! i
            | j <- [0 .. indexCount index1 - 1]]
            | i <- [0 .. indexCount index2 - 1]]
    | otherwise = t1
switchInd (Err msg) _ = Err msg

-- Push index given as Maybe String into the deepest level of recursion
switchInd' :: Num a => Tensor a -> Maybe String -> Tensor a
switchInd' (Scalar x) _ = Scalar x
switchInd' t1 Nothing = t1
switchInd' t1@(Tensor _ _) i@(Just _)
    | length (indices t1) > 1 =
        let t2 = switchInd t1 i
        in Tensor (tensorIndex t2) $ (`switchInd'` i) <$> tensorData t2
    | otherwise = t1
switchInd' (Err msg) _ = Err msg

-- Dot product of covector and vector (specifically in this order)
-- Number of elements in two tensors must be the same
_dot :: Num a => Tensor a -> Tensor a -> Tensor a
Scalar x1 `_dot` Scalar x2 =  Scalar $ x1 * x2
Tensor i1@(Covariant count1 _) ts1 `_dot` Tensor i2@(Contravariant count2 _) ts2
    | count1 == count2 = sum $ zipWith (*) ts1 ts2
    | otherwise = Err $
        "Tensor product: " ++ incompatibleTypes ++
        " - index1 is " ++ show i1 ++
        " and index2 is " ++ show i2
t1 `_dot` t2 = Err $
    "Tensor product: " ++ incompatibleTypes ++
    " - index1 is " ++ show (tensorIndex t1) ++
    " and index2 is " ++ show (tensorIndex t2)

-- Tensor product with Einstein summation convention
-- Does not optimize tensor structure (summed indices on deepest level of recursion)
(!*!) :: Num a => Tensor a -> Tensor a -> Tensor a
(Scalar x1) !*! (Scalar x2) = Scalar $ x1 * x2
(Scalar x) !*! t2 = (x *) <$> t2
t1 !*! (Scalar x) = (* x) <$> t1
t1@(Tensor index1 ts1) !*! t2@(Tensor index2 ts2)
    | indexName index1 == indexName index2 = t1 `_dot` t2
    | indexName index1 `elem` (indexName <$> indices t2) =
        Tensor index2 [t1 !*! (ts2 !! i) | i <- [1 .. indexCount index2] ]
    | otherwise = Tensor index1 [(ts1 !! i) !*! t2 | i <- [1 .. indexCount index1] ]
Err msg !*! _ = Err msg
_ !*! Err msg = Err msg

-- Tensor product with Einstein summation convention
-- Optimizes tensor structure for efficiency - pushes summed indices at the deepest level of recursion
(!*) :: Num a => Tensor a -> Tensor a -> Tensor a
(Scalar x1) !* (Scalar x2) = Scalar $ x1 * x2
(Scalar x) !* t2 = (x *) <$> t2
t1 !* (Scalar x) = (* x) <$> t1
t1@(Tensor _ _) !* t2@(Tensor _ _) =
    let cmi = commonIndex t1 t2
    in switchInd' t1 cmi !*! switchInd' t2 cmi
Err msg !* _ = Err msg
_ !* Err msg = Err msg

-- Find common index in two tensors, if any
commonIndex :: Num a => Tensor a -> Tensor a -> Maybe String
commonIndex t1@(Tensor _ _) t2@(Tensor _ _) =
    let indicesNames1 = indexName <$> indices t1
        indicesNames2 = indexName <$> indices t2
    in msum $ (\i -> find (==i) indicesNames2) <$> indicesNames1
commonIndex _ _ = Nothing


