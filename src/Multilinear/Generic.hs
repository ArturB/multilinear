{-|
Module      : Multilinear.Generic.AsArray
Description : Generic array tensor
Copyright   : (c) Artur M. Brodzki, 2017
License     : GPL-3
Maintainer  : artur.brodzki@gmail.com
Stability   : experimental
Portability : Windows/POSIX

- This module contains implementation of tensor defined as nested list of its components.
- Choice of container type has great impact on library performance in particular use cases
- Array ("Data.Vector") implementation is generally faster, however it is strict and always keeps all tensor elements in memory, so it may require large amount of RAM.
- List implementation is slower but lazy and when tensor is generated from indices or randomly, it does not generate all elements at once if not necessary,
so it may operate in smaller memory (e.g. linear instead of quadratic when multiplying matrix by vector or form).

-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Multilinear.Generic (
    Tensor(..), (!), mergeScalars,
    isScalar, isSimple, isFiniteTensor, isInfiniteTensor
) where

import           Codec.Compression.GZip
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Maybe
import           Data.Aeson
import           Data.Bits
import qualified Data.ByteString.Lazy       as ByteString
import           Data.Foldable
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Serialize
import qualified Data.Vector                as Boxed
import           Data.Vector.Serialize      ()
import           GHC.Generics
import           Multilinear
import qualified Multilinear.Index          as Index
import qualified Multilinear.Index.Finite   as Finite
import qualified Multilinear.Index.Infinite as Infinite

{-| ERROR MESSAGES -}
incompatibleTypes :: String
incompatibleTypes = "Incompatible tensor types!"


scalarIndices :: String
scalarIndices = "Scalar has no indices!"

differentIndices :: String
differentIndices = "Tensors have different indices!"

infiniteIndex :: String
infiniteIndex = "Index is infinitely-dimensional!"

{-| Tensor defined recursively as scalar or list of other tensors -}
{-| @c@ is type of a container, @i@ is type of index size and @a@ is type of tensor elements -}
data Tensor a =
    {-| Scalar -}
    Scalar {
        {-| value of scalar -}
        scalarVal :: a
    } |
    {-| Simple, one-dimensional finite tensor -}
    SimpleFinite {
        tensorFiniteIndex :: Finite.Index,
        tensorScalars     :: Boxed.Vector a
    } |
    {-| Finite array of other tensors -}
    FiniteTensor {
        {-| Finite index "Mutltilinear.Index.Finite" of tensor -}
        tensorFiniteIndex :: Finite.Index,
        {-| Array of tensors on deeper recursion level -}
        tensorsFinite     :: Boxed.Vector (Tensor a)
    } |
    {-| Infinite list of other tensors -}
    InfiniteTensor {
        {-| Infinite index "Mutltilinear.Index.Infinite" of tensor -}
        tensorInfiniteIndex :: Infinite.Index,
        {-| Infinite list of tensors on deeper recursion level -}
        tensorsInfinite     :: [Tensor a]
    } |
    {-| Operations on tensors may throw an error -}
    Err {
        {-| Error message -}
        errMessage :: String
    } deriving (Eq, Generic)

{-| Return true if tensor is a scalar -}
isScalar :: Tensor a -> Bool
isScalar (Scalar _) = True
isScalar _          = False

{-| Return true if tensor is a simple tensor -}
isSimple :: Tensor a -> Bool
isSimple (SimpleFinite _ _) = True
isSimple _                  = False

{-| Return True if tensor is a complex tensor -}
isFiniteTensor :: Tensor a -> Bool
isFiniteTensor (FiniteTensor _ _) = True
isFiniteTensor _                  = False

{-| Return True if tensor is a infinite tensor -}
isInfiniteTensor :: Tensor a -> Bool
isInfiniteTensor (InfiniteTensor _ _) = True
isInfiniteTensor _                    = False

{-| Return generic tensor index -}
tensorIndex :: Tensor a -> Index.TIndex
tensorIndex (Scalar _)           = error scalarIndices
tensorIndex (SimpleFinite i _)   = Index.toTIndex i
tensorIndex (FiniteTensor i _)   = Index.toTIndex i
tensorIndex (InfiniteTensor i _) = Index.toTIndex i
tensorIndex (Err msg)            = error msg

{-| Return True if tensor has no elements -}
isEmptyTensor :: Tensor a -> Bool
isEmptyTensor (Scalar _)            = False
isEmptyTensor (SimpleFinite _ ts)   = Boxed.null ts
isEmptyTensor (FiniteTensor _ ts)   = Boxed.null ts
isEmptyTensor (InfiniteTensor _ ts) = null ts
isEmptyTensor (Err _)               = True

{-| Returns sample element of the tensor. Used to determine some features common for all elements, like bit-qualities. -}
firstElem :: Tensor a -> a
firstElem (Scalar x)            = x
firstElem (SimpleFinite _ ts)   = Boxed.head ts
firstElem (FiniteTensor _ ts)   = firstElem $ Boxed.head ts
firstElem (InfiniteTensor _ ts) = firstElem $ head ts
firstElem (Err msg)             = error msg

{-| Recursive indexing on list tensor
    @t ! i = t[i]@
-}
(!) :: Tensor a      -- ^ tensor @t@
    -> Int           -- ^ index @i@
    -> Tensor a      -- ^ tensor @t[i]@

(!) (Scalar _) _            = Err scalarIndices
(!) (Err msg) _             = Err msg
(!) (SimpleFinite _  ts) i  = Scalar $ ts Boxed.! i
(!) (FiniteTensor _  ts) i  = ts Boxed.! i
(!) (InfiniteTensor _ ts) i = ts !! i

-- Binary serialization instance
instance Serialize a => Serialize (Tensor a)
-- JSON serialization instance
instance ToJSON a => ToJSON (Tensor a)
-- JSON deserialization instance
instance FromJSON a => FromJSON (Tensor a)

-- Print tensor
instance (
    Show a
    ) => Show (Tensor a) where

    -- merge errors first and then print whole tensor
    show t = show' $ _mergeErr t
        where
        -- Scalar is showed simply as its value
        show' (Scalar x) = show x
        -- Covariant components are shown horizontally
        show' (FiniteTensor index@(Finite.Covariant _ _) ts) =
            show index ++ " T: " ++ _showHorizontal ts
        show' (SimpleFinite index@(Finite.Covariant _ _) ts) =
            show index ++ " S: " ++ _showHorizontal ts
        -- Contravariant components are shown vertically
        show' (FiniteTensor index@(Finite.Contravariant _ _) ts)=
            show index ++ " T: " ++ _showVertical ts
        show' (SimpleFinite index@(Finite.Contravariant _ _) ts)=
            show index ++ " S: " ++ _showVertical ts
        -- Sequences are shown horizontally
        show' (FiniteTensor index@(Finite.Indifferent _ _) ts) =
            show index ++ " T: " ++ _showHorizontal ts
        show' (SimpleFinite index@(Finite.Indifferent _ _) ts) =
            show index ++ " S: " ++ _showHorizontal ts
        -- Error prints it error message
        show' (Err msg) = show msg

        -- Merge many errors in tensor to the first one
        -- Error tensor is passed further
        _mergeErr (Err msg) = Err msg
        -- Tensor is merged to first error on deeper recursion level
        _mergeErr t1@(FiniteTensor _ ts) =
            -- find first error if present
            let err = Data.List.find _isErrTensor (_mergeErr <$> ts)
            -- and return this error if found, whole tensor otherwise
            in fromMaybe t1 err
        -- in other types of tensor cannot be any error
        _mergeErr t1 = t1

        -- return True if tensor is an error
        --_isErrTensor :: Tensor c a -> Bool
        _isErrTensor (Err _) = True
        _isErrTensor _       = False

        -- print container elements vertically
        -- used to show contravariant components of tensor, which by convention are written vertically
        _showVertical :: (Show a, Foldable c) => c a -> String
        _showVertical container =
            "\n" ++ tail (foldl' (\string e -> string ++ "\n  |" ++ show e) "" container)

        -- print container elements horizontally
        -- used to show contravariant components of tensor, which by convention are written vertically
        _showHorizontal :: (Show a, Foldable c) => c a -> String
        _showHorizontal container =
            "[" ++ tail (foldl' (\string e -> string ++ "," ++ show e) "" container) ++ "]"

-- Tensor is a functor
instance Functor Tensor where
    -- Mapping scalars simply maps its value
    fmap f (Scalar v_)              = Scalar (f v_)
    -- Mapping vectors does mapping element by element
    fmap f (SimpleFinite indexT ts) = SimpleFinite indexT (f <$> ts)
    -- Mapping tensors does mapping element by element
    fmap f (FiniteTensor indexT ts) = FiniteTensor indexT $ fmap (fmap f) ts
    -- Mapping errors changes nothing
    fmap _ (Err msg)                = Err msg

-- Tensors can be compared lexigographically
-- Allowes to put tensors in typical ordered containers
instance (
    Ord a
    ) => Ord (Tensor a) where

    -- Error is smaller by other tensors, so when printing ordered containers, all erorrs will be printed first
    -- Two errors are compared by they messages lexigographically
    Err msg1 <= Err msg2 = msg1 <= msg2
    Err _ <= _ = True
    _ <= Err _ = False
    -- Scalar is smaller than any complex tensor
    -- Two scalars are compared by they values
    Scalar x1 <= Scalar x2 = x1 <= x2
    Scalar _ <= _ = True
    _ <= Scalar _ = False
    -- Complex tensors are compared lexigographically
    SimpleFinite _ ts1 <= SimpleFinite _ ts2 = ts1 <= ts2
    FiniteTensor _ ts1 <= FiniteTensor _ ts2 = ts1 <= ts2
    SimpleFinite _ _ <= FiniteTensor _ _     = True
    FiniteTensor _ _ <= SimpleFinite _ _     = False

-- Tensors concatenation makes them a monoid
instance (
    Num a, Bits a
    ) => Monoid (Tensor a) where
    -- Neutral element is a scalar as it has no indices and concatenation is by common inidces
    mempty = {-FiniteTensor (Finite.Indifferent (Just 0) "i") mempty-} Scalar 0

    -- Tensor concatenation by common index
    mappend t1@(FiniteTensor ti1 ts1) t2@(FiniteTensor _ ts2) =
        if indices t1 == indices t2
        then FiniteTensor ti1 $ ts1 <> ts2
        else Err differentIndices
    mappend t1@(SimpleFinite ti1 ts1) t2@(SimpleFinite _ ts2) =
        if indices t1 == indices t2
        then SimpleFinite ti1 $ ts1 <> ts2
        else Err differentIndices
    mappend (SimpleFinite _ _) t2@(FiniteTensor _ _) = t2
    mappend (FiniteTensor _ _) t2@(SimpleFinite _ _) = t2
    mappend (Err msg) _ = Err msg
    mappend _ (Err msg) = Err msg
    mappend (Scalar _) t = t
    mappend t (Scalar _) = t

{-| Merge FiniteTensor of Scalars to SimpleFinite tensor for performance improvement -}
mergeScalars :: Tensor a -> Tensor a
mergeScalars (FiniteTensor index1 ts1) = case ts1 Boxed.! 0 of
    Scalar _ -> SimpleFinite index1 (scalarVal <$> ts1)
    _        -> FiniteTensor index1 $ mergeScalars <$> ts1
mergeScalars t = t

{-| Apply a tensor operator elem by elem -}
_elemByElem' :: (Num a, Bits a) =>
               Tensor a                                    -- ^ First argument of operator
            -> Tensor a                                    -- ^ Second argument of operator
            -> (a -> a -> a)                                   -- ^ Operator on tensor elements if indices are different
            -> (Tensor a -> Tensor a -> Tensor a)  -- ^ Tensor operator called if indices are the same
            -> Tensor a                                    -- ^ Result tensor

_elemByElem' (Scalar x1) (Scalar x2) f _ = Scalar $ f x1 x2
_elemByElem' (Scalar x) t f _ = (x `f`) <$> t
_elemByElem' t (Scalar x) f _ = (`f` x) <$> t

_elemByElem' t1@(FiniteTensor index1 v1) t2@(FiniteTensor index2 v2) f op
    | Index.indexName index1 == Index.indexName index2 = op t1 t2
    | Index.indexName index1 `Data.List.elem` indicesNames t2 =
        FiniteTensor index2 $ (\x -> _elemByElem' t1 x f op) <$> v2
    | otherwise = FiniteTensor index1 $ (\x -> _elemByElem' x t2 f op) <$> v1

_elemByElem' t1@(SimpleFinite index1 v1) t2@(SimpleFinite index2 _) f op
    | Index.indexName index1 == Index.indexName index2 = op t1 t2
    | otherwise = FiniteTensor index1 $ (\x -> f x <$> t2) <$> v1

_elemByElem' t1@(SimpleFinite index1 _) t2@(FiniteTensor index2 v2) f op
    | Index.indexName index1 == Index.indexName index2 = op t1 t2
    | otherwise = FiniteTensor index2 $ (\x -> _elemByElem' t1 x f op) <$> v2

_elemByElem' t1@(FiniteTensor index1 v1) t2@(SimpleFinite index2 _) f op
    | Index.indexName index1 == Index.indexName index2 = op t1 t2
    | otherwise = FiniteTensor index1 $ (\x -> _elemByElem' x t2 f op) <$> v1

_elemByElem' (Err msg) _ _ _ = Err msg
_elemByElem' _ (Err msg) _ _ = Err msg

{-| Apply a tensor operator elem by elem merging scalars at the and -}
_elemByElem :: (Num a, Bits a) =>
               Tensor a                                    -- ^ First argument of operator
            -> Tensor a                                    -- ^ Second argument of operator
            -> (a -> a -> a)                                   -- ^ Operator on tensor elements if indices are different
            -> (Tensor a -> Tensor a -> Tensor a)  -- ^ Tensor operator called if indices are the same
            -> Tensor a                                    -- ^ Result tensor
_elemByElem t1 t2 f op = mergeScalars $ _elemByElem' t1 t2 f op

-- Zipping two tensors by a function, assuming they have the same indices
zipT :: (
    Num a, Bits a
    ) => (Tensor a -> Tensor a -> Tensor a)   -- ^ Two tensors combinator
      -> (Tensor a -> a -> Tensor a)              -- ^ Tensor and scalar combinator
      -> (a -> Tensor a -> Tensor a)              -- ^ Scalar and tensor combinator
      -> (a -> a -> a)                                    -- ^ Two scalars combinator
      -> Tensor a                                     -- ^ First tensor to zip
      -> Tensor a                                     -- ^ Second tensor to zip
      -> Tensor a                                     -- ^ Result tensor

zipT f _ _ _ (FiniteTensor index v1) (FiniteTensor _ v2) = FiniteTensor index $ f <$> v1 <*> v2
zipT _ f _ _ (FiniteTensor index v1) (SimpleFinite _ v2) = FiniteTensor index $ f <$> v1 <*> v2
zipT _ _ f _ (SimpleFinite index v1) (FiniteTensor _ v2) = FiniteTensor index $ f <$> v1 <*> v2
zipT _ _ _ f (SimpleFinite index v1) (SimpleFinite _ v2) = SimpleFinite index $ f <$> v1 <*> v2
zipT _ _ _ _ _ _ = Err "zipT"

-- dot product of two tensors
dot :: (
    Num a, Bits a
    ) => (Tensor a -> Tensor a -> Tensor a)   -- ^ Two tensors multiplication
      -> (Tensor a -> a -> Tensor a)              -- ^ Tensor and scalar multiplication
      -> (a -> Tensor a -> Tensor a)              -- ^ Scalar and tensor multiplication
      -> (a -> a -> a)                                    -- ^ Two scalars multiplication
      -> (Tensor a -> Tensor a -> Tensor a)   -- ^ Tensor summation
      -> (a -> a -> a)                                    -- ^ Scalars summation
      -> Tensor a                                     -- ^ First dot product argument
      -> Tensor a                                     -- ^ Second dot product argument
      -> Tensor a                                     -- ^ Resulting dot product

dot f _ _ _ add _ (FiniteTensor i1@(Finite.Covariant count1 _) ts1') (FiniteTensor i2@(Finite.Contravariant count2 _) ts2')
    | count1 == count2 = Data.Foldable.foldl1 add $ Boxed.zipWith f ts1' ts2'
    | otherwise = contractionErr (Index.toTIndex i1) (Index.toTIndex i2)
dot _ _ _ f _ add (SimpleFinite i1@(Finite.Covariant count1 _) ts1') (SimpleFinite i2@(Finite.Contravariant count2 _) ts2')
    | count1 == count2 = Scalar $ Data.Foldable.foldl1 add $ Boxed.zipWith f ts1' ts2'
    | otherwise = contractionErr (Index.toTIndex i1) (Index.toTIndex i2)
dot _ _ f _ add _ (SimpleFinite i1@(Finite.Covariant count1 _) ts1') (FiniteTensor i2@(Finite.Contravariant count2 _) ts2')
    | count1 == count2 = Data.Foldable.foldl1 add $ Boxed.zipWith f ts1' ts2'
    | otherwise = contractionErr (Index.toTIndex i1) (Index.toTIndex i2)
dot _ f _ _ add _ (FiniteTensor i1@(Finite.Covariant count1 _) ts1') (SimpleFinite i2@(Finite.Contravariant count2 _) ts2')
    | count1 == count2 = Data.Foldable.foldl1 add $ Boxed.zipWith f ts1' ts2'
    | otherwise = contractionErr (Index.toTIndex i1) (Index.toTIndex i2)
dot _ _ _ _ _ _ t1' t2' = contractionErr (tensorIndex t1') (tensorIndex t2')

-- contraction error
contractionErr :: Index.TIndex   -- ^ Index of first dot product parameter
               -> Index.TIndex   -- ^ Index of second dot product parameter
               -> Tensor a       -- ^ Erorr message

contractionErr i1' i2' = Err $
    "Tensor product: " ++ incompatibleTypes ++
    " - index1 is " ++ show i1' ++
    " and index2 is " ++ show i2'

-- Tensors can be added, subtracted and multiplicated
instance (
    Num a, Bits a
    ) => Num (Tensor a) where

    -- Adding - element by element
    t1 + t2 = _elemByElem t1 t2 (+) $ zipT (+) (+.) (.+) (+)

    -- Subtracting - element by element
    t1 - t2 = _elemByElem t1 t2 (-) $ zipT (-) (-.) (.-) (-)

    -- Multiplicating is treated as tensor product
    -- Tensor product applies Einstein summation convention
    t1 * t2 = _elemByElem t1 t2 (*) $ dot (*) (*.) (.*) (*) (+) (+)

    -- Absolute value - element by element
    abs t = abs <$> t

    -- Signum operation - element by element
    signum t = signum <$> t

    -- Simple integer can be conveted to Scalar
    fromInteger x = Scalar $ fromInteger x

-- Bit operations on tensors
instance (
    Num a, Bits a
    ) => Bits (Tensor a) where

    -- Bit sum - elem by elem
    t1 .|. t2 = _elemByElem t1 t2 (.|.) $ zipT (.|.) (\t e -> (.|. e) <$> t) (\e t -> (e .|.) <$> t) (.|.)

    -- Bit tensor product
    -- Summation and multiplication are replaced by its bit equivalents
    -- Two scalars are multiplicated by their values
    t1 .&. t2 = _elemByElem t1 t2 (.&.) $ dot (.&.) (\t e -> (.&. e) <$> t) (\e t -> (e .&.) <$> t) (.&.) (.|.) (.|.)

    -- Bit exclusive sum (XOR) - elem by elem
    t1 `xor` t2 = _elemByElem t1 t2 xor $ zipT xor (\t e -> (`xor` e) <$> t) (\e t -> (e `xor`) <$> t) xor

    -- Bit complement
    complement = Multilinear.map complement

    -- Bit shift of all elements
    shift t n = Multilinear.map (`shift` n) t

    -- Bit rotating of all elements
    rotate t n = Multilinear.map (`rotate` n) t

    -- Returns number of bits of elements of tensor, -1 for elements of undefined size
    bitSize (Scalar x)          = fromMaybe (-1) $ bitSizeMaybe x
    bitSize (Err _)             = -1
    bitSize t =
        if isEmptyTensor t
        then (-1)
        else fromMaybe (-1) $ bitSizeMaybe $ firstElem t

    -- Returns number of bits of elements of tensor
    bitSizeMaybe (Scalar x)          = bitSizeMaybe x
    bitSizeMaybe (Err _)             = Nothing
    bitSizeMaybe t =
        if isEmptyTensor t
        then Nothing
        else bitSizeMaybe $ firstElem t

    -- Returns true if tensors element are signed
    isSigned (Scalar x)          = isSigned x
    isSigned (Err _)             = False
    isSigned t =
        not (isEmptyTensor t) &&
        isSigned (firstElem t)

    -- bit i is a scalar value with the ith bit set and all other bits clear.
    bit i = Scalar (bit i)

    -- Test bit - shoud retur True, if bit n if equal to 1.
    -- Tensors are entities with many elements, so this function always returns False.
    -- Do not use it, it is implemented only for legacy purposes.
    testBit _ _ = False

    -- Return the number of set bits in the argument. This number is known as the population count or the Hamming weight.
    popCount = popCountDefault

-- Tensors can be divided by each other
instance (
    Fractional a, Bits a
    ) => Fractional (Tensor a) where

    -- Scalar division return result of division of its values
    Scalar x1 / Scalar x2 = Scalar $ x1 / x2
    -- Tensor and scalar are divided value by value
    Scalar x1 / t2 = (x1 /) <$> t2
    t1 / Scalar x2 = (/ x2) <$> t1
    Err msg / _ = Err msg
    _ / Err msg = Err msg
    -- Two complex tensors cannot be (for now) simply divided
    -- // TODO - tensor division and inversion
    _ / _ = Err "TODO"

    -- A scalar can be generated from rational number
    fromRational x = Scalar $ fromRational x

-- Real-number functions on tensors.
-- Function of tensor is tensor of function of its elements
-- E.g. exp [1,2,3,4] = [exp 1, exp2, exp3, exp4]
instance (
    Floating a, Bits a
    ) => Floating (Tensor a) where

    {-| PI number -}
    pi = Scalar pi

    {-| Exponential function. (exp t)[i] = exp( t[i] ) -}
    exp t = exp <$> t

    {-| Natural logarithm. (log t)[i] = log( t[i] ) -}
    log t = log <$> t

    {-| Sinus. (sin t)[i] = sin( t[i] ) -}
    sin t = sin <$> t

    {-| Cosinus. (cos t)[i] = cos( t[i] ) -}
    cos t = cos <$> t

    {-| Inverse sinus. (asin t)[i] = asin( t[i] ) -}
    asin t = asin <$> t

    {-| Inverse cosinus. (acos t)[i] = acos( t[i] ) -}
    acos t = acos <$> t

    {-| Inverse tangent. (atan t)[i] = atan( t[i] ) -}
    atan t = atan <$> t

    {-| Hyperbolic sinus. (sinh t)[i] = sinh( t[i] ) -}
    sinh t = sinh <$> t

    {-| Hyperbolic cosinus. (cosh t)[i] = cosh( t[i] ) -}
    cosh t = cosh <$> t

    {-| Inverse hyperbolic sinus. (asinh t)[i] = asinh( t[i] ) -}
    asinh t = acosh <$> t

    {-| Inverse hyperbolic cosinus. (acosh t)[i] = acosh (t[i] ) -}
    acosh t = acosh <$> t

    {-| Inverse hyperbolic tangent. (atanh t)[i] = atanh( t[i] ) -}
    atanh t = atanh <$> t

-- Multilinear operations
instance (
    Num a, Bits a
    ) => Multilinear Tensor a where

    -- Add scalar left
    x .+ t = (x+) <$> t

    -- Subtract scalar left
    x .- t = (x-) <$> t

    -- Multiplicate by scalar left
    x .* t = (x*) <$> t

    -- Add scalar right
    t +. x = (+x) <$> t

    -- Subtract scalar right
    t -. x = (\p -> p - x) <$> t

    -- Multiplicate by scalar right
    t *. x = (*x) <$> t

    -- List of all tensor indices
    indices (Scalar _)          = []
    indices (FiniteTensor i ts) = Index.toTIndex i : indices (head $ toList ts)
    indices (SimpleFinite i _)  = [Index.toTIndex i]
    indices (Err _)             = []

    -- Get tensor order [ (contravariant,covariant)-type ]
    order (Scalar _) = (0,0)

    order (FiniteTensor (Finite.Contravariant _ _) t) = (cnvr+1,covr)
        where (cnvr,covr) = order $ Data.List.head (toList t)
    order (FiniteTensor (Finite.Covariant _ _) t) = (cnvr,covr+1)
        where (cnvr,covr) = order $ Data.List.head (toList t)
    order (FiniteTensor (Finite.Indifferent _ _) t) = (cnvr,covr)
        where (cnvr,covr) = order $ Data.List.head (toList t)

    order (SimpleFinite (Finite.Contravariant _ _) _) = (1,0)
    order (SimpleFinite (Finite.Covariant _ _) _) = (0,1)
    order (SimpleFinite (Finite.Indifferent _ _) _) = (0,0)

    order (Err _) = (-1,-1)

    -- Get size of tensor index or Nothing if index is infinite or tensor
    size (Scalar _)             = Left scalarIndices
    size (SimpleFinite index _) = Right $ Finite.indexSize index
    size (FiniteTensor index _) = Right $ Finite.indexSize index
    size (InfiniteTensor _ _)   = Left infiniteIndex
    size (Err msg)              = Left msg

    -- Rename tensor index
    rename (Scalar x) _ _ = Scalar x

    rename (FiniteTensor i@(Finite.Contravariant count name) ts) before after
        | name == before = FiniteTensor (Finite.Contravariant count after) $ (\t' -> rename t' before after) <$> ts
        | otherwise = FiniteTensor i $ (\t' -> rename t' before after) <$> ts
    rename (FiniteTensor i@(Finite.Covariant count name) ts) before after
        | name == before = FiniteTensor (Finite.Covariant count after) $ (\t' -> rename t' before after) <$> ts
        | otherwise = FiniteTensor i $ (\t' -> rename t' before after) <$> ts
    rename (FiniteTensor i@(Finite.Indifferent count name) ts) before after
        | name == before = FiniteTensor (Finite.Indifferent count after) $ (\t' -> rename t' before after) <$> ts
        | otherwise = FiniteTensor i $ (\t' -> rename t' before after) <$> ts

    rename t1@(SimpleFinite (Finite.Contravariant count name) ts) before after
        | name == before = SimpleFinite (Finite.Contravariant count after) ts
        | otherwise = t1
    rename t1@(SimpleFinite (Finite.Covariant count name) ts) before after
        | name == before = SimpleFinite (Finite.Covariant count after) ts
        | otherwise = t1
    rename t1@(SimpleFinite (Finite.Indifferent count name) ts) before after
        | name == before = SimpleFinite (Finite.Indifferent count after) ts
        | otherwise = t1

    rename (Err msg) _ _ = Err msg

    -- Raise an index
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
    Err msg /\ _ = Err msg

    -- Lower an index
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
    Err msg \/ _ = Err msg

    {-| Transpose a tensor (switch all indices types) -}
    transpose (Scalar x) = Scalar x

    transpose (FiniteTensor (Finite.Covariant count name) ts) =
        FiniteTensor (Finite.Contravariant count name) (Multilinear.transpose <$> ts)
    transpose (FiniteTensor (Finite.Contravariant count name) ts) =
        FiniteTensor (Finite.Covariant count name) (Multilinear.transpose <$> ts)
    transpose (FiniteTensor (Finite.Indifferent count name) ts) =
        FiniteTensor (Finite.Indifferent count name) (Multilinear.transpose <$> ts)

    transpose (SimpleFinite (Finite.Covariant count name) ts) =
        SimpleFinite (Finite.Contravariant count name) ts
    transpose (SimpleFinite (Finite.Contravariant count name) ts) =
        SimpleFinite (Finite.Covariant count name) ts
    transpose (SimpleFinite (Finite.Indifferent count name) ts) =
        SimpleFinite (Finite.Indifferent count name) ts

    transpose (Err msg) = Err msg

    {-| Concatenation of two tensor with given index or by creating a new one -}
    augment t1 t2 ind =
        let t1' = t1 <<<| ind
            t2' = t2 <<<| ind
        in  t1' <> t2'

    {-| Shift tensor index right -}
    {-| Moves given index one level deeper in recursion -}
    -- Error tensor has no indices to shift
    Err msg |>> _  = Err msg
    -- Scalar has no indices to shift
    Scalar x |>> _ = Scalar x
    -- Simple tensor has only one index which cannot be shifted
    t1@(SimpleFinite _ _) |>> _ = t1
    --
    t1@(FiniteTensor index1 ts1) |>> ind
        -- We don't shift this index
        | Data.List.length (indicesNames t1) > 1 && Index.indexName index1 /= ind =
            FiniteTensor index1 $ (|>> ind) <$> ts1
        -- We found index to shift
        | Data.List.length (indicesNames t1) > 1 && Index.indexName index1 == ind =
                -- Next index
            let index2 = tensorFiniteIndex (ts1 Boxed.! 0)
                -- Elements to transpose
                dane = if isSimple (ts1 Boxed.! 0)
                       then (Scalar <$>) <$> (tensorScalars <$> ts1)
                       else tensorsFinite <$> ts1
                -- Convert to list
                daneList = Boxed.toList <$> Boxed.toList dane
                -- and transpose tensor data (standard function available only for list)
                transposedList = Data.List.transpose daneList
                -- then reconvert to vector again
                transposed = Boxed.fromList <$> Boxed.fromList transposedList
            -- and reconstruct tensor with transposed elements
            in  mergeScalars $ FiniteTensor index2 $ FiniteTensor index1 <$> transposed
        | otherwise = t1

    {-| Serialize to binary string -}
    toBinary = Data.Serialize.encodeLazy

    {-| Write to binary file. Uses compression with gzip -}
    toBinaryFile name = ByteString.writeFile name . compress . toBinary

    {-| Deserialize from binary string -}
    fromBinary = Data.Serialize.decodeLazy

    {-| Read from binary file -}
    fromBinaryFile name = do
        contents <- lift $ ByteString.readFile name
        EitherT $ return $ fromBinary $ decompress contents


    {-| Serialize to JSON string -}
    toJSON = Data.Aeson.encode

    {-| Write to JSON file -}
    toJSONFile name = ByteString.writeFile name . Multilinear.toJSON

    {-| Deserialize from JSON string -}
    fromJSON = Data.Aeson.decode

    {-| Read from JSON file -}
    fromJSONFile name = do
        contents <- lift $ ByteString.readFile name
        MaybeT $ return $ Multilinear.fromJSON contents


{-| List allows for random access to tensor elements -}
instance (
    Num a, Bits a
    ) => Accessible Tensor a where

    {-| Accessing tensor elements -}
    el _ (Scalar x) _ = Scalar x
    el inds t1@(SimpleFinite index1 _) vals =
        let indvals = zip inds vals
            val = Data.List.find (\(n,_) -> n == Index.indexName index1) indvals
        in  if isJust val
            then t1 ! snd (fromJust val)
            else t1
    el inds t1@(FiniteTensor index1 v1) vals =
        let indvals = zip inds vals
            val = Data.List.find (\(n,_) -> n == Index.indexName index1) indvals
            indvals1 = Data.List.filter (\(n,_) -> n /= Index.indexName index1) indvals
            inds1 = fst $ unzip indvals1
            vals1 = snd $ unzip indvals1
        in  if isJust val
            then el inds1 (t1 ! snd (fromJust val)) vals1
            else FiniteTensor index1 $ (\t -> el inds t vals) <$> v1
    el _ (Err msg) _ = Err msg

    {-| Mapping with indices. -}
    iMap f t = iMap' t zeroList
        where
        zeroList = Data.List.replicate 100 0

        iMap' (Scalar x) inds =
            Scalar $ f inds x
        iMap' (SimpleFinite index ts) inds =
            SimpleFinite index $ Boxed.imap (\i e -> f (inds ++ [i]) e) ts
        iMap' (FiniteTensor index ts) inds =
            FiniteTensor index $ Boxed.imap (\i e -> iMap' e (inds ++ [i])) ts
        iMap' (Err msg) _  =
            Err msg

