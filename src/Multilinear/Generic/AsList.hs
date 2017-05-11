{-|
Module      : Multilinear.Generic.AsList
Description : Generic list tensor
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
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Multilinear.Generic.AsList (
    Tensor(..), (!), mergeScalars,
    isScalar, isSimple, isFiniteTensor
) where

import           Codec.Compression.GZip
import           Control.Applicative
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Maybe
import           Data.Aeson
import           Data.Bits
import qualified Data.ByteString.Lazy       as ByteString
import           Data.Foldable
import           Data.Hashable
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Serialize
import           GHC.Generics
import           Multilinear
import           Multilinear.Generic
import           Multilinear.Index

{-| ERROR MESSAGES -}
incompatibleTypes :: String
incompatibleTypes = "Incompatible tensor types!"

scalarIndices :: String
scalarIndices = "Scalar has no indices!"

differentIndices :: String
differentIndices = "Tensors have different indices!"

{-| Tensor defined recursively as scalar or list of other tensors -}
{-| @c@ is type of a container, @i@ is type of index size and @a@ is type of tensor elements -}
data instance Tensor ZipList a =
    {-| Scalar -}
    Scalar {
        {-| value of scalar -}
        scalarVal :: a
    } |
    {-| Simple Finite -}
    SimpleFinite {
        tensorIndex :: TIndex,
        tensorScalars :: ZipList a
    } |
    {-| Container of other tensors -}
    FiniteTensor {
        {-| Finite index "Mutltilinear.Index.Finite" of tensor -}
        tensorIndex :: TIndex,
        {-| Containter of tensors on deeper recursion level -}
        tensorData  :: ZipList (Tensor ZipList a)
    } |
    {-| Operations on tensors may throw an error -}
    Err {
        {-| Error message -}
        errMessage :: String
    } deriving (Eq, Generic)

{-| Return true if tensor is a scalar -}
isScalar :: ListTensor a -> Bool
isScalar (Scalar _) = True
isScalar _          = False

{-| Return true if tensor is a simple tensor -}
isSimple :: ListTensor a -> Bool
isSimple (SimpleFinite _ _) = True
isSimple _                  = False

{-| Return True if tensor is a complex tensor -}
isFiniteTensor :: ListTensor a -> Bool
isFiniteTensor (FiniteTensor _ _) = True
isFiniteTensor _                  = False


{-|
    Recursive indexing on list tensor
    @t ! i = t[i]@
-}
(!) :: ListTensor a      -- ^ tensor @t@
    -> Int               -- ^ index @i@
    -> ListTensor a      -- ^ tensor @t[i]@

(!) (Scalar _) _                    = Err scalarIndices
(!) (Err msg) _                     = Err msg
(!) (SimpleFinite _ (ZipList ts)) i = Scalar $ ts !! i
(!) (FiniteTensor _ (ZipList ts)) i = ts !! i

{-| Binary serialization and deserialization -}
instance (Serialize a) => Serialize (ZipList a)
instance (
    Serialize a
    ) => Serialize (ListTensor a)

{-| Serialization to and from JSON -}
instance FromJSON a => FromJSON (ZipList a)
instance (
    FromJSON a
    ) => FromJSON (ListTensor a)

instance ToJSON a => ToJSON (ZipList a)
instance (
    ToJSON a
    ) =>   ToJSON (ListTensor a)

-- Print tensor
instance (
    Show a
    ) => Show (ListTensor a) where

    -- merge errors first and then print whole tensor
    show t = show' $ _mergeErr t
        where
        -- Scalar is showed simply as its value
        show' (Scalar x) = show x
        -- Covariant components are shown horizontally
        show' (FiniteTensor index@(Covariant _ _) ts) =
            show index ++ " T: " ++ _showHorizontal ts
        show' (SimpleFinite index@(Covariant _ _) ts) =
            show index ++ " S: " ++ _showHorizontal ts
        -- Contravariant components are shown vertically
        show' (FiniteTensor index@(Contravariant _ _) ts)=
            show index ++ " T: " ++ _showVertical ts
        show' (SimpleFinite index@(Contravariant _ _) ts)=
            show index ++ " S: " ++ _showVertical ts
        -- Sequences are shown horizontally
        show' (FiniteTensor index@(Indifferent _ _) ts) =
            show index ++ " T: " ++ _showHorizontal ts
        show' (SimpleFinite index@(Indifferent _ _) ts) =
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
instance Functor ListTensor where
    -- Mapping scalars simply maps its value
    fmap f (Scalar v_) = Scalar (f v_)
    -- Mapping vectors does mapping element by element
    fmap f (SimpleFinite indexT ts) = SimpleFinite indexT (f <$> ts)
    -- Mapping tensors does mapping element by element
    fmap f (FiniteTensor indexT (ZipList ts)) = FiniteTensor indexT $ ZipList $ fmap (fmap f) ts
    -- Mapping errors changes nothing
    fmap _ (Err msg) = Err msg

-- Tensors can be compared lexigographically
-- Allowes to put tensors in typical ordered containers
instance (
    Ord a
    ) => Ord (ListTensor a) where

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

-- You can compute a hash value from tensor
-- Allows to put tensors to typical unordered containers
instance Hashable a => Hashable (ZipList a)
instance (
    Hashable a
    ) => Hashable (ListTensor a)

-- Tensors concatenation makes them a monoid
instance (
    Num a, Bits a
    ) => Monoid (ListTensor a) where
    -- Neutral element is a scalar as it has no indices and concatenation is by common inidces
    mempty = {-FiniteTensor (Indifferent (Just 0) "i") mempty-} Scalar 0

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
mergeScalars :: ListTensor a -> ListTensor a
mergeScalars (FiniteTensor index1 (ZipList ts1)) = case head ts1 of
    Scalar _ -> SimpleFinite index1 $ ZipList (scalarVal <$> ts1)
    _        -> FiniteTensor index1 $ ZipList $ mergeScalars <$> ts1
mergeScalars t = t

{-| Apply a tensor operator elem by elem -}
_elemByElem' :: (Num a, Bits a) =>
               ListTensor a                                    -- ^ First argument of operator
            -> ListTensor a                                    -- ^ Second argument of operator
            -> (a -> a -> a)                                   -- ^ Operator on tensor elements if indices are different
            -> (ListTensor a -> ListTensor a -> ListTensor a)  -- ^ Tensor operator called if indices are the same
            -> ListTensor a                                    -- ^ Result tensor

_elemByElem' (Scalar x1) (Scalar x2) f _ = Scalar $ f x1 x2
_elemByElem' (Scalar x) t f _ = (x `f`) <$> t
_elemByElem' t (Scalar x) f _ = (`f` x) <$> t

_elemByElem' t1@(FiniteTensor index1 v1) t2@(FiniteTensor index2 v2) f op
    | indexName index1 == indexName index2 = op t1 t2
    | indexName index1 `Data.List.elem` indicesNames t2 =
        FiniteTensor index2 $ (\x -> _elemByElem' t1 x f op) <$> v2
    | otherwise = FiniteTensor index1 $ (\x -> _elemByElem' x t2 f op) <$> v1

_elemByElem' t1@(SimpleFinite index1 v1) t2@(SimpleFinite index2 _) f op
    | indexName index1 == indexName index2 = op t1 t2
    | otherwise = FiniteTensor index1 $ (\x -> f x <$> t2) <$> v1

_elemByElem' t1@(SimpleFinite index1 _) t2@(FiniteTensor index2 v2) f op
    | indexName index1 == indexName index2 = op t1 t2
    | otherwise = FiniteTensor index2 $ (\x -> _elemByElem' t1 x f op) <$> v2

_elemByElem' t1@(FiniteTensor index1 v1) t2@(SimpleFinite index2 _) f op
    | indexName index1 == indexName index2 = op t1 t2
    | otherwise = FiniteTensor index1 $ (\x -> _elemByElem' x t2 f op) <$> v1

_elemByElem' (Err msg) _ _ _ = Err msg
_elemByElem' _ (Err msg) _ _ = Err msg

{-| Apply a tensor operator elem by elem merging scalars at the and -}
_elemByElem :: (Num a, Bits a) =>
               ListTensor a                                    -- ^ First argument of operator
            -> ListTensor a                                    -- ^ Second argument of operator
            -> (a -> a -> a)                                   -- ^ Operator on tensor elements if indices are different
            -> (ListTensor a -> ListTensor a -> ListTensor a)  -- ^ Tensor operator called if indices are the same
            -> ListTensor a                                    -- ^ Result tensor
_elemByElem t1 t2 f op = mergeScalars $ _elemByElem' t1 t2 f op

-- Zipping two tensors by a function, assuming they have the same indices
zipT :: (
    Num a, Bits a
    ) => (ListTensor a -> ListTensor a -> ListTensor a)   -- ^ Two tensors combinator
      -> (ListTensor a -> a -> ListTensor a)              -- ^ Tensor and scalar combinator
      -> (a -> ListTensor a -> ListTensor a)              -- ^ Scalar and tensor combinator
      -> (a -> a -> a)                                    -- ^ Two scalars combinator
      -> ListTensor a                                     -- ^ First tensor to zip
      -> ListTensor a                                     -- ^ Second tensor to zip
      -> ListTensor a                                     -- ^ Result tensor

zipT f _ _ _ (FiniteTensor index v1) (FiniteTensor _ v2) = FiniteTensor index $ f <$> v1 <*> v2
zipT _ f _ _ (FiniteTensor index v1) (SimpleFinite _ v2) = FiniteTensor index $ f <$> v1 <*> v2
zipT _ _ f _ (SimpleFinite index v1) (FiniteTensor _ v2) = FiniteTensor index $ f <$> v1 <*> v2
zipT _ _ _ f (SimpleFinite index v1) (SimpleFinite _ v2) = SimpleFinite index $ f <$> v1 <*> v2
zipT _ _ _ _ _ _ = Err "zipT"

-- dot product of two tensors
dot :: (
    Num a, Bits a
    ) => (ListTensor a -> ListTensor a -> ListTensor a)   -- ^ Two tensors multiplication
      -> (ListTensor a -> a -> ListTensor a)              -- ^ Tensor and scalar multiplication
      -> (a -> ListTensor a -> ListTensor a)              -- ^ Scalar and tensor multiplication
      -> (a -> a -> a)                                    -- ^ Two scalars multiplication
      -> (ListTensor a -> ListTensor a -> ListTensor a)   -- ^ Tensor summation
      -> (a -> a -> a)                                    -- ^ Scalars summation
      -> ListTensor a                                     -- ^ First dot product argument
      -> ListTensor a                                     -- ^ Second dot product argument
      -> ListTensor a                                     -- ^ Resulting dot product

dot f _ _ _ add _ (FiniteTensor i1@(Covariant count1 _) ts1') (FiniteTensor i2@(Contravariant count2 _) ts2')
    | count1 == count2 = Data.Foldable.foldl1 add $ f <$> ts1' <*> ts2'
    | otherwise = contractionErr i1 i2
dot _ _ _ f _ add (SimpleFinite i1@(Covariant count1 _) ts1') (SimpleFinite i2@(Contravariant count2 _) ts2')
    | count1 == count2 = Scalar $ Data.Foldable.foldl1 add $ f <$> ts1' <*> ts2'
    | otherwise = contractionErr i1 i2
dot _ _ f _ add _ (SimpleFinite i1@(Covariant count1 _) ts1') (FiniteTensor i2@(Contravariant count2 _) ts2')
    | count1 == count2 = Data.Foldable.foldl1 add $ f <$> ts1' <*> ts2'
    | otherwise = contractionErr i1 i2
dot _ f _ _ add _ (FiniteTensor i1@(Covariant count1 _) ts1') (SimpleFinite i2@(Contravariant count2 _) ts2')
    | count1 == count2 = Data.Foldable.foldl1 add $ f <$> ts1' <*> ts2'
    | otherwise = contractionErr i1 i2
dot _ _ _ _ _ _ t1' t2' = contractionErr (tensorIndex t1') (tensorIndex t2')

-- contraction error
contractionErr :: TIndex        -- ^ Index of first dot product parameter
               -> TIndex        -- ^ Index of second dot product parameter
               -> ListTensor a  -- ^ Erorr message
 
contractionErr i1' i2' = Err $
    "Tensor product: " ++ incompatibleTypes ++
    " - index1 is " ++ show i1' ++
    " and index2 is " ++ show i2'

-- Tensors can be added, subtracted and multiplicated
instance (
    Num a, Bits a
    ) => Num (ListTensor a) where

    -- Adding - element by element
    t1 + t2 = _elemByElem t1 t2 (+) $ zipT (+) (+.) (.+) (+)

    -- Subtracting - element by element
    t1 - t2 = _elemByElem t1 t2 (-) $ zipT (-) (-.) (.-) (-)

    -- Multiplicating is treated as tensor product
    -- Tensor product applies Einstein summation convention
    -- Two scalars are multiplicated by their values
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
    ) => Bits (ListTensor a) where

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
        if null $ tensorData t
        then (-1)
        else let firstElem = head $ toList (tensorData t)
             in  fromMaybe (-1) $ bitSizeMaybe firstElem

    -- Returns number of bits of elements of tensor
    bitSizeMaybe (Scalar x)          = bitSizeMaybe x
    bitSizeMaybe (Err _)             = Nothing
    bitSizeMaybe t =
        if null $ tensorData t
        then Nothing
        else let firstElem = head $ toList (tensorData t)
             in  bitSizeMaybe firstElem

    -- Returns true if tensors element are signed
    isSigned (Scalar x)          = isSigned x
    isSigned (Err _)             = False
    isSigned t =
        not (null (tensorData t)) &&
        let firstElem = head $ toList (tensorData t)
        in  isSigned firstElem

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
    ) => Fractional (ListTensor a) where

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
    ) => Floating (ListTensor a) where

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
    ) => Multilinear ListTensor a where

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
    indices (FiniteTensor i ts) = toTIndex i : indices (head $ toList ts)
    indices (SimpleFinite i _)  = [i]
    indices (Err _)             = []

    -- Get tensor order [ (contravariant,covariant)-type ]
    order (Scalar _) = (0,0)

    order (FiniteTensor (Contravariant _ _) t) = (cnvr+1,covr)
        where (cnvr,covr) = order $ Data.List.head (toList t)
    order (FiniteTensor (Covariant _ _) t) = (cnvr,covr+1)
        where (cnvr,covr) = order $ Data.List.head (toList t)
    order (FiniteTensor (Indifferent _ _) t) = (cnvr,covr)
        where (cnvr,covr) = order $ Data.List.head (toList t)

    order (SimpleFinite (Contravariant _ _) _) = (1,0)
    order (SimpleFinite (Covariant _ _) _) = (0,1)
    order (SimpleFinite (Indifferent _ _) _) = (0,0)

    order (Err _) = (-1,-1)

    -- Rename tensor index
    rename (Scalar x) _ _ = Scalar x

    rename (FiniteTensor i@(Contravariant count name) ts) before after
        | name == before = FiniteTensor (Contravariant count after) $ (\t' -> rename t' before after) <$> ts
        | otherwise = FiniteTensor i $ (\t' -> rename t' before after) <$> ts
    rename (FiniteTensor i@(Covariant count name) ts) before after
        | name == before = FiniteTensor (Covariant count after) $ (\t' -> rename t' before after) <$> ts
        | otherwise = FiniteTensor i $ (\t' -> rename t' before after) <$> ts
    rename (FiniteTensor i@(Indifferent count name) ts) before after
        | name == before = FiniteTensor (Indifferent count after) $ (\t' -> rename t' before after) <$> ts
        | otherwise = FiniteTensor i $ (\t' -> rename t' before after) <$> ts

    rename t1@(SimpleFinite (Contravariant count name) ts) before after
        | name == before = SimpleFinite (Contravariant count after) ts
        | otherwise = t1
    rename t1@(SimpleFinite (Covariant count name) ts) before after
        | name == before = SimpleFinite (Covariant count after) ts
        | otherwise = t1
    rename t1@(SimpleFinite (Indifferent count name) ts) before after
        | name == before = SimpleFinite (Indifferent count after) ts
        | otherwise = t1

    rename (Err msg) _ _ = Err msg

    -- Raise an index
    Scalar x /\ _ = Scalar x
    FiniteTensor index ts /\ n
        | indexName index == n =
            FiniteTensor (Contravariant (indexSize index) n) $ (/\ n) <$> ts
        | otherwise =
            FiniteTensor index $ (/\ n) <$> ts
    t1@(SimpleFinite index ts) /\ n
        | indexName index == n =
            SimpleFinite (Contravariant (indexSize index) n) ts
        | otherwise = t1
    Err msg /\ _ = Err msg

    -- Lower an index
    Scalar x \/ _ = Scalar x
    FiniteTensor index ts \/ n
        | indexName index == n =
            FiniteTensor (Covariant (indexSize index) n) $ (\/ n) <$> ts
        | otherwise =
            FiniteTensor index $ (\/ n) <$> ts
    t1@(SimpleFinite index ts) \/ n
        | indexName index == n =
            SimpleFinite (Covariant (indexSize index) n) ts
        | otherwise = t1
    Err msg \/ _ = Err msg

    {-| Transpose a tensor (switch all indices types) -}
    transpose (Scalar x) = Scalar x

    transpose (FiniteTensor (Covariant count name) ts) =
        FiniteTensor (Contravariant count name) (Multilinear.transpose <$> ts)
    transpose (FiniteTensor (Contravariant count name) ts) =
        FiniteTensor (Covariant count name) (Multilinear.transpose <$> ts)
    transpose (FiniteTensor (Indifferent count name) ts) =
        FiniteTensor (Indifferent count name) (Multilinear.transpose <$> ts)

    transpose (SimpleFinite (Covariant count name) ts) =
        SimpleFinite (Contravariant count name) ts
    transpose (SimpleFinite (Contravariant count name) ts) =
        SimpleFinite (Covariant count name) ts
    transpose (SimpleFinite (Indifferent count name) ts) =
        SimpleFinite (Indifferent count name) ts

    transpose (Err msg) = Err msg

    {-| Concatenation of two tensor with given index or by creating a new one -}
    augment t1 t2 ind =
        let t1' = t1 <<<| ind
            t2' = t2 <<<| ind
        in  t1' <> t2'

    {-| Shift tensor index right -}
    {-| Moves given index one level deeper in recursion -}
    Err msg |>> _  = Err msg
    Scalar x |>> _ = Scalar x
    t1@(SimpleFinite _ _) |>> _ = t1
    t1@(FiniteTensor index1 (ZipList ts1)) |>> ind
        | Data.List.length (indicesNames t1) > 1 && indexName index1 /= ind =
            FiniteTensor index1 $ ZipList $ (|>> ind) <$> ts1
        | Data.List.length (indicesNames t1) > 1 && indexName index1 == ind =
            let index2 = tensorIndex (Data.List.head ts1)
                dane = if isSimple (head ts1)
                       then (Scalar <$>) <$> (getZipList . tensorScalars <$> ts1)
                       else (getZipList . tensorData) <$> ts1
                transposed = ZipList <$> Data.List.transpose dane
            in  mergeScalars $ FiniteTensor index2 $ ZipList $ FiniteTensor index1 <$> transposed
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
    ) => Accessible ListTensor a where

    {-| Accessing tensor elements -}
    el _ (Scalar x) _ = Scalar x
    el inds t1@(SimpleFinite index1 _) vals =
        let indvals = zip inds vals
            val = Data.List.find (\(n,_) -> n == indexName index1) indvals
        in  if isJust val
            then t1 ! snd (fromJust val)
            else t1
    el inds t1@(FiniteTensor index1 (ZipList v1)) vals =
        let indvals = zip inds vals
            val = Data.List.find (\(n,_) -> n == indexName index1) indvals
            indvals1 = Data.List.filter (\(n,_) -> n /= indexName index1) indvals
            inds1 = fst $ unzip indvals1
            vals1 = snd $ unzip indvals1
        in  if isJust val
            then el inds1 (t1 ! snd (fromJust val)) vals1
            else FiniteTensor index1 $ ZipList $ (\t -> el inds t vals) <$> v1
    el _ (Err msg) _ = Err msg

    {-| Mapping with indices. -}
    iMap f t = iMap' t zeroList
        where
        zeroList = Data.List.replicate 100 0

        iMap' (Scalar x) inds =
            Scalar $ f inds x
        iMap' (SimpleFinite index (ZipList ts)) inds =
            SimpleFinite index $ ZipList $ (f . (\ i -> inds ++ [i]) <$> [0 ..]) <*> ts
        iMap' (FiniteTensor index (ZipList ts)) inds =
            FiniteTensor index $ ZipList $ (\tind -> iMap' (fst tind) $ inds ++ [snd tind]) <$> zip ts [0..]
        iMap' (Err msg) _  =
            Err msg

