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
    Tensor(..), (!),
    toBinary, toBinaryFile,
    fromBinary, fromBinaryFile,
    Multilinear.Generic.AsList.toJSON, toJSONFile,
    Multilinear.Generic.AsList.fromJSON, fromJSONFile,
    mergeScalars
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
--import qualified Data.Set                   as Set
import           GHC.Generics
import           Multilinear
import           Multilinear.Generic
import           Multilinear.Index

{-| ERROR MESSAGES -}
incompatibleTypes :: String
incompatibleTypes = "Incompatible tensor types!"

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

{-|
    Recursive indexing on list tensor
    @t ! i = t[i]@
-}
(!) :: ListTensor a      -- ^ tensor @t@
    -> Int               -- ^ index @i@
    -> ListTensor a      -- ^ tensor @t[i]@

(!) (Scalar _) _                    = Err "Scalar has no indices!"
(!) (Err msg) _                     = Err msg
--(!) (SimpleFinite _ (ZipList ts)) i = Scalar $ ts !! i
(!) (FiniteTensor _ (ZipList ts)) i = ts !! i

{-| Binary serialization and deserialization -}
instance (Serialize a) => Serialize (ZipList a)
instance (
    Serialize a
    ) => Serialize (ListTensor a)

{-| Serialize to binary string -}
toBinary :: (
    Serialize a
    ) => ListTensor a            -- ^ Tensor to serialize
      -> ByteString.ByteString   -- ^ Tensor serialized to binary string
toBinary = Data.Serialize.encodeLazy

{-| Write to binary file. Uses compression with gzip -}
toBinaryFile :: (
    Serialize a
    ) => String         -- ^ File name
      -> ListTensor a   -- ^ Tensor to serialize
      -> IO ()
toBinaryFile name = ByteString.writeFile name . compress . toBinary

{-| Deserialize from binary string -}
fromBinary :: (
    Serialize a
    ) => ByteString.ByteString          -- ^ Binary string to deserialize
      -> Either String (ListTensor a)   -- ^ Deserialized tensor or deserialization error message
fromBinary = Data.Serialize.decodeLazy

{-| Read from binary file -}
fromBinaryFile :: (
    Serialize a
    ) => String                             -- ^ File name
      -> EitherT String IO (ListTensor a)   -- ^ Deserialized tensor or deserialization error message - either way, wrapped in the IO monad
fromBinaryFile name = do
    contents <- lift $ ByteString.readFile name
    EitherT $ return $ fromBinary $ decompress contents

{-| Serialization to and from JSON -}
instance FromJSON a => FromJSON (ZipList a)
instance (
    FromJSON a
    ) => FromJSON (ListTensor a)

instance ToJSON a => ToJSON (ZipList a)
instance (
    ToJSON a
    ) =>   ToJSON (ListTensor a)

{-| Serialize to JSON string -}
toJSON :: (
    ToJSON a
    ) => ListTensor a            -- ^ Tensor to serialize
      -> ByteString.ByteString   -- ^ Binary string with JSON-encoded tensor
toJSON = Data.Aeson.encode

{-| Write to JSON file -}
toJSONFile :: (
    ToJSON a
    ) => String          -- ^ File name
      -> ListTensor a    -- ^ Tensor to serialize
      -> IO ()
toJSONFile name = ByteString.writeFile name . Multilinear.Generic.AsList.toJSON

{-| Deserialize from JSON string -}
fromJSON :: (
    FromJSON a
    ) => ByteString.ByteString    -- ^ Binary string with JSON-encoded tensor
      -> Maybe (ListTensor a)     -- ^ Deserialized tensor or an error
fromJSON = Data.Aeson.decode

{-| Read from JSON file -}
fromJSONFile :: (
    FromJSON a
    ) => String                      -- ^ File name
      -> MaybeT IO (ListTensor a)    -- ^ Deserialized tensor or an error, either way wrapped in the IO monad
fromJSONFile name = do
    contents <- lift $ ByteString.readFile name
    MaybeT $ return $ Multilinear.Generic.AsList.fromJSON contents

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
    fmap f (FiniteTensor indexT (ZipList ts)) = case head ts of
        Scalar _         -> FiniteTensor indexT $ ZipList [Scalar (f x) | Scalar x <- ts]
        FiniteTensor _ _ -> FiniteTensor indexT $ ZipList $ fmap (fmap f) ts
        Err msg          -> Err msg
    --fmap f (FiniteTensor indexT (ZipList ts)) = FiniteTensor indexT $ ZipList $ fmap (fmap f) ts
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
    --SimpleFinite _ ts1 <= SimpleFinite _ ts2 = ts1 <= ts2
    FiniteTensor _ ts1 <= FiniteTensor _ ts2 = ts1 <= ts2
    --SimpleFinite _ _ <= FiniteTensor _ _     = True
    --FiniteTensor _ _ <= SimpleFinite _ _     = False

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
    mempty = FiniteTensor (Indifferent (Just 0) "i") mempty

    -- Tensor concatenation by common index
    mappend t1@(FiniteTensor ti1 ts1) t2@(FiniteTensor _ ts2) =
        if indices t1 == indices t2
        then FiniteTensor ti1 $ ts1 <> ts2
        else Err "Tensors have different indices!"
    --mappend (SimpleFinite _ _) (FiniteTensor _ _) = Err "Scalars and tensors cannot be concatenated!"
    --mappend (FiniteTensor _ _) (SimpleFinite _ _) = Err "Scalars and tensors cannot be concatenated!"
    mappend (Err msg) _ = Err msg
    mappend _ (Err msg) = Err msg
    mappend (Scalar _) _ = Err "Scalars cannot be concatenated!"
    mappend _ (Scalar _) = Err "Cannot concatenate by scalar!"

mergeScalars :: ListTensor a -> ListTensor a
mergeScalars (FiniteTensor index1 (ZipList ts1)) = case head ts1 of
    Scalar _ -> SimpleFinite index1 $ ZipList (scalarVal <$> ts1)
    _        -> FiniteTensor index1 $ ZipList $ mergeScalars <$> ts1
mergeScalars t = t

-- Tensors can be added, subtracted and multiplicated
instance (
    Num a, Bits a
    ) => Num (ListTensor a) where

    -- Adding - element by element
    Scalar x1 + Scalar x2 = Scalar $ x1 + x2
    Scalar x + t = (x+) <$> t
    t + Scalar x = (+x) <$> t
    t1@(FiniteTensor index1 v1) + t2@(FiniteTensor index2 v2)
        | index1 == index2 = FiniteTensor index1 $ (+) <$> v1 <*> v2
        | indexName index1 `Data.List.elem` indicesNames t2 =
            FiniteTensor index2 $ (t1+) <$> tensorData t2
        | otherwise = FiniteTensor index1 $ (+t2) <$> tensorData t1
    t1@(SimpleFinite index1 v1) + t2@(SimpleFinite index2 v2)
        | index1 == index2 = SimpleFinite index1 $ (+) <$> v1 <*> v2
        | indexName index1 `Data.List.elem` indicesNames t2 =
            FiniteTensor index2 $ (t1+) <$> tensorData t2
        | otherwise = FiniteTensor index1 $ (+t2) <$> tensorData t1
    t1@(SimpleFinite index1 v1) + t2@(FiniteTensor index2 v2)
        | index1 == index2 = FiniteTensor index1 $ (.+) <$> v1 <*> v2
        | indexName index1 `Data.List.elem` indicesNames t2 =
            FiniteTensor index2 $ (t1+) <$> tensorData t2
        | otherwise = FiniteTensor index1 $ (+t2) <$> tensorData t1
    t1@(FiniteTensor index1 v1) + t2@(SimpleFinite index2 v2)
        | index1 == index2 = FiniteTensor index1 $ (+.) <$> v1 <*> v2
        | indexName index1 `Data.List.elem` indicesNames t2 =
            FiniteTensor index2 $ (t1+) <$> tensorData t2
        | otherwise = FiniteTensor index1 $ (+t2) <$> tensorData t1
    Err msg + _ = Err msg
    _ + Err msg = Err msg

    -- Subtracting - element by element
    Scalar x1 - Scalar x2 = Scalar $ x1 - x2
    Scalar x - t = (\e -> x - e) <$> t
    t - Scalar x = (\e -> e - x) <$> t
    t1@(FiniteTensor index1 v1) - t2@(FiniteTensor index2 v2)
        | index1 == index2 = FiniteTensor index1 $ (-) <$> v1 <*> v2
        | indexName index1 `Data.List.elem` indicesNames t2 =
            FiniteTensor index2 $ (t1-) <$> tensorData t2
        | otherwise = FiniteTensor index1 $ (\x -> x - t2) <$> tensorData t1
    Err msg - _ = Err msg
    _ - Err msg = Err msg

    -- Multiplicating is treated as tensor product
    -- Tensor product applies Einstein summation convention
    -- Two scalars are multiplicated by their values
    Scalar x1 * Scalar x2 = Scalar $ x1 * x2
    -- Multiplicate by scalar is simply a map
    Scalar x1 * t = (x1*) <$> t
    t * Scalar x2 = (*x2) <$> t
    -- Two tensors may be contracted or multiplicated elem by elem
    t1@(FiniteTensor index1 _) * t2@(FiniteTensor index2 _)
        | indexName index1 == indexName index2 = t1 `dot` t2
        | indexName index1 `Data.List.elem` indicesNames t2 =
            FiniteTensor index2 $ (t1 *) <$> tensorData t2
        | otherwise = FiniteTensor index1 $ (* t2) <$> tensorData t1
        where
        -- Contraction of covariant and contravariant index
        FiniteTensor i1@(Covariant count1 _) ts1' `dot` FiniteTensor i2@(Contravariant count2 _) ts2'
            | count1 == count2 = sum $ (*) <$> ts1' <*> ts2'
            | otherwise = contractionErr i1 i2
        t1' `dot` t2' = contractionErr (tensorIndex t1') (tensorIndex t2')
        contractionErr i1' i2' = Err $
                "Tensor product: " ++ incompatibleTypes ++
                " - index1 is " ++ show i1' ++
                " and index2 is " ++ show i2'
    t1@(SimpleFinite index1 ts1) * t2@(SimpleFinite index2 ts2)
        | indexName index1 == indexName index2 = Scalar $ sum $ (*) <$> ts1 <*> ts2
        | indexName index1 `Data.List.elem` indicesNames t2 =
            FiniteTensor index2 $ (t1 *) <$> tensorData t2
        | otherwise = FiniteTensor index1 $ (* t2) <$> tensorData t1
        where
        -- Contraction of covariant and contravariant index
        SimpleFinite i1@(Covariant count1 _) ts1' `dot` SimpleFinite i2@(Contravariant count2 _) ts2'
            | count1 == count2 = Scalar $ sum $ (*) <$> ts1' <*> ts2'
            | otherwise = contractionErr i1 i2
        t1' `dot` t2' = contractionErr (tensorIndex t1') (tensorIndex t2')
        contractionErr i1' i2' = Err $
                "Tensor product: " ++ incompatibleTypes ++
                " - index1 is " ++ show i1' ++
                " and index2 is " ++ show i2'
    t1@(SimpleFinite index1 _) * t2@(FiniteTensor index2 _)
        | indexName index1 == indexName index2 = t1 `dot` t2
        | indexName index1 `Data.List.elem` indicesNames t2 =
            FiniteTensor index2 $ (t1 *) <$> tensorData t2
        | otherwise = FiniteTensor index1 $ (* t2) <$> tensorData t1
        where
        -- Contraction of covariant and contravariant index
        SimpleFinite i1@(Covariant count1 _) ts1' `dot` FiniteTensor i2@(Contravariant count2 _) ts2'
            | count1 == count2 = sum $ (.*) <$> ts1' <*> ts2'
            | otherwise = contractionErr i1 i2
        t1' `dot` t2' = contractionErr (tensorIndex t1') (tensorIndex t2')
        contractionErr i1' i2' = Err $
                "Tensor product: " ++ incompatibleTypes ++
                " - index1 is " ++ show i1' ++
                " and index2 is " ++ show i2'
    t1@(FiniteTensor index1 _) * t2@(SimpleFinite index2 _)
        | indexName index1 == indexName index2 = t1 `dot` t2
        | indexName index1 `Data.List.elem` indicesNames t2 =
            FiniteTensor index2 $ (t1 *) <$> tensorData t2
        | otherwise = FiniteTensor index1 $ (* t2) <$> tensorData t1
        where
        -- Contraction of covariant and contravariant index
        FiniteTensor i1@(Covariant count1 _) ts1' `dot` SimpleFinite i2@(Contravariant count2 _) ts2'
            | count1 == count2 = sum $ (*.) <$> ts1' <*> ts2'
            | otherwise = contractionErr i1 i2
        t1' `dot` t2' = contractionErr (tensorIndex t1') (tensorIndex t2')
        contractionErr i1' i2' = Err $
                "Tensor product: " ++ incompatibleTypes ++
                " - index1 is " ++ show i1' ++
                " and index2 is " ++ show i2'
    -- Multiplicating by error simply pushed this error forward
    Err msg * _ = Err msg
    _ * Err msg = Err msg

    -- Absolute value - element by element
    abs (Scalar x)              = Scalar $ abs x
    abs (FiniteTensor index ts) = FiniteTensor index (abs <$> ts)
    abs (Err msg)               = Err msg

    -- Signum operation - element by element
    signum (Scalar x)              = Scalar $ signum x
    signum (FiniteTensor index ts) = FiniteTensor index (signum <$> ts)
    signum (Err msg)               = Err msg

    -- Simple integer can be conveted to Scalar
    fromInteger x = Scalar $ fromInteger x

-- Bit operations on tensors
instance (
    Num a, Bits a
    ) => Bits (ListTensor a) where

    -- Bit sum - elem by elem
    Scalar x1 .|. Scalar x2 = Scalar $ x1 .|. x2
    Scalar x .|. t = (x .|.) <$> t
    t .|. Scalar x = (.|. x) <$> t
    t1@(FiniteTensor index1 v1) .|. t2@(FiniteTensor index2 v2)
        | index1 == index2 = FiniteTensor index1 $ (.|.) <$> v1 <*> v2
        | indexName index1 `Data.List.elem` indicesNames t2 =
            FiniteTensor index2 $ (t1 .|.) <$> tensorData t2
        | otherwise = FiniteTensor index1 $ (.|. t2) <$> tensorData t1
    Err msg .|. _ = Err msg
    _ .|. Err msg = Err msg

    -- Bit tensor product
    -- Summation and multiplication are replaced by its bit equivalents
    -- Two scalars are multiplicated by their values
    Scalar x1 .&. Scalar x2 = Scalar $ x1 .&. x2
    -- Multiplicate by scalar is simply a map
    Scalar x1 .&. t = (x1 .&.) <$> t
    t .&. Scalar x2 = (.&. x2) <$> t
    -- Two tensors may be contracted or multiplicated elem by elem
    t1@(FiniteTensor index1 _) .&. t2@(FiniteTensor index2 _)
        | index1 == index2 = t1 `dot` t2
        | indexName index1 `Data.List.elem` indicesNames t2 =
            FiniteTensor index2 $ (t1 .&.) <$> tensorData t2
        | otherwise = FiniteTensor index1 $ (.&. t2) <$> tensorData t1
        where
        -- Contraction of covariant and contravariant index
        FiniteTensor i1@(Covariant count1 _) ts1' `dot` FiniteTensor i2@(Contravariant count2 _) ts2'
            | count1 == count2 = foldl' (.|.) 0 $ (.&.) <$> ts1' <*> ts2'
            | otherwise = contractionErr i1 i2
        _ `dot` _ = Err "Cannot compute a bit dot product!"
        contractionErr i1' i2' = Err $
                "Tensor bit product: " ++ incompatibleTypes ++
                " - index1 is " ++ show i1' ++
                " and index2 is " ++ show i2'
    -- Multiplicating by error simply pushed this error futher
    Err msg .&. _ = Err msg
    _ .&. Err msg = Err msg

    -- Bit exclusive sum (XOR) - elem by elem
    Scalar x1 `xor` Scalar x2 = Scalar $ x1 `xor` x2
    Scalar x `xor` t = (x `xor`) <$> t
    t `xor` Scalar x = (`xor` x) <$> t
    t1@(FiniteTensor index1 v1) `xor` t2@(FiniteTensor index2 v2)
        | index1 == index2 = FiniteTensor index1 $ xor <$> v1 <*> v2
        | indexName index1 `Data.List.elem` indicesNames t2 =
            FiniteTensor index2 $ (t1 `xor`) <$> tensorData t2
        | otherwise = FiniteTensor index1 $ (`xor` t2) <$> tensorData t1
    Err msg `xor` _ = Err msg
    _ `xor` Err msg = Err msg

    -- Bit complement
    complement = Multilinear.map complement

    -- Bit shift of all elements
    shift t n = Multilinear.map (`shift` n) t

    -- Bit rotating of all elements
    rotate t n = Multilinear.map (`rotate` n) t

    -- Returns number of bits of elements of tensor, -1 for elements of undefined size
    bitSize (Scalar x)          = fromMaybe (-1) $ bitSizeMaybe x
    bitSize (FiniteTensor _ ts) =
        if null ts
        then (-1)
        else let firstElem = head $ toList ts
             in  fromMaybe (-1) $ bitSizeMaybe firstElem
    bitSize (Err _)             = -1

    -- Returns number of bits of elements of tensor
    bitSizeMaybe (Scalar x)          = bitSizeMaybe x
    bitSizeMaybe (FiniteTensor _ ts) =
        if null ts
        then Nothing
        else let firstElem = head $ toList ts
             in  bitSizeMaybe firstElem
    bitSizeMaybe (Err _)             = Nothing

    -- Returns true if tensors element are signed
    isSigned (Scalar x)          = isSigned x
    isSigned (FiniteTensor _ ts) =
        not (null ts) &&
        let firstElem = head $ toList ts
        in  isSigned firstElem
    isSigned (Err _)             = False

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
    -- Two complex tensors cannot be (for now) simply divided
    -- // TODO - tensor division and inversion
    t1@(FiniteTensor _ _) / t2@(FiniteTensor _ _) = Err $
         "(/): " ++ incompatibleTypes ++
        " tensor1 has type " ++ show (indicesNames t1) ++
        " and tensor2 has type " ++ show (indicesNames t2)
    Err msg / _ = Err msg
    _ / Err msg = Err msg

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
    exp (Scalar x)              = Scalar $ exp x
    exp (FiniteTensor index ts) = FiniteTensor index (exp <$> ts)
    exp (Err msg)               = Err msg

    {-| Natural logarithm. (log t)[i] = log( t[i] ) -}
    log (Scalar x)              = Scalar $ log x
    log (FiniteTensor index ts) = FiniteTensor index (log <$> ts)
    log (Err msg)               = Err msg

    {-| Sinus. (sin t)[i] = sin( t[i] ) -}
    sin (Scalar x)              = Scalar $ sin x
    sin (FiniteTensor index ts) = FiniteTensor index (sin <$> ts)
    sin (Err msg)               = Err msg

    {-| Cosinus. (cos t)[i] = cos( t[i] ) -}
    cos (Scalar x)              = Scalar $ cos x
    cos (FiniteTensor index ts) = FiniteTensor index (cos <$> ts)
    cos (Err msg)               = Err msg

    {-| Inverse sinus. (asin t)[i] = asin( t[i] ) -}
    asin (Scalar x)              = Scalar $ asin x
    asin (FiniteTensor index ts) = FiniteTensor index (asin <$> ts)
    asin (Err msg)               = Err msg

    {-| Inverse cosinus. (acos t)[i] = acos( t[i] ) -}
    acos (Scalar x)              = Scalar $ acos x
    acos (FiniteTensor index ts) = FiniteTensor index (acos <$> ts)
    acos (Err msg)               = Err msg

    {-| Inverse tangent. (atan t)[i] = atan( t[i] ) -}
    atan (Scalar x)              = Scalar $ atan x
    atan (FiniteTensor index ts) = FiniteTensor index (atan <$> ts)
    atan (Err msg)               = Err msg

    {-| Hyperbolic sinus. (sinh t)[i] = sinh( t[i] ) -}
    sinh (Scalar x)              = Scalar $ sinh x
    sinh (FiniteTensor index ts) = FiniteTensor index (sinh <$> ts)
    sinh (Err msg)               = Err msg

    {-| Hyperbolic cosinus. (cosh t)[i] = cosh( t[i] ) -}
    cosh (Scalar x)              = Scalar $ cosh x
    cosh (FiniteTensor index ts) = FiniteTensor index (cosh <$> ts)
    cosh (Err msg)               = Err msg

    {-| Inverse hyperbolic sinus. (asinh t)[i] = asinh( t[i] ) -}
    asinh (Scalar x)              = Scalar $ asinh x
    asinh (FiniteTensor index ts) = FiniteTensor index (asinh <$> ts)
    asinh (Err msg)               = Err msg

    {-| Inverse hyperbolic cosinus. (acosh t)[i] = acosh (t[i] ) -}
    acosh (Scalar x)              = Scalar $ acosh x
    acosh (FiniteTensor index ts) = FiniteTensor index (acosh <$> ts)
    acosh (Err msg)               = Err msg

    {-| Inverse hyperbolic tangent. (atanh t)[i] = atanh( t[i] ) -}
    atanh (Scalar x)              = Scalar $ atanh x
    atanh (FiniteTensor index ts) = FiniteTensor index (atanh <$> ts)
    atanh (Err msg)               = Err msg

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
    rename (Err msg) _ _ = Err msg

    -- Raise an index
    Scalar x /\ _ = Scalar x
    FiniteTensor index ts /\ n
        | indexName index == n =
            FiniteTensor (Contravariant (indexSize index) n) $ (/\ n) <$> ts
        | otherwise =
            FiniteTensor index $ (/\ n) <$> ts
    Err msg /\ _ = Err msg

    -- Lower an index
    Scalar x \/ _ = Scalar x
    FiniteTensor index ts \/ n
        | indexName index == n =
            FiniteTensor (Covariant (indexSize index) n) $ (\/ n) <$> ts
        | otherwise =
            FiniteTensor index $ (\/ n) <$> ts
    Err msg \/ _ = Err msg

    {-| Transpose a tensor (switch all indices types) -}
    transpose (Scalar x) = Scalar x
    transpose (FiniteTensor (Covariant count name) ts) =
        FiniteTensor (Contravariant count name) (Multilinear.transpose <$> ts)
    transpose (FiniteTensor (Contravariant count name) ts) =
        FiniteTensor (Covariant count name) (Multilinear.transpose <$> ts)
    transpose (FiniteTensor (Indifferent count name) ts) =
        FiniteTensor (Indifferent count name) (Multilinear.transpose <$> ts)
    transpose (Err msg) = Err msg

    {-| Mapping with indices. -}
    {-iMap f t = iMap' t zeroList
        where
        zeroList = 0:zeroList

        iMap' (Scalar x) inds =
            Scalar $ f inds x
        iMap' (FiniteTensor index ts) inds =
            Tensor index (\tind -> iMap' (fst tind) $ inds ++ [snd tind]) <$> zip ts [0..]
        iMap' (Err msg) _  =
            Err msg -}

    {-| Concatenation of two tensor with given index or by creating a new one -}
    augment t1 t2 ind =
        let t1' = t1 <<<| ind
            t2' = t2 <<<| ind
        in  t1' <> t2'

    {-| Shift tensor index right -}
    {-| Moves given index one level deeper in recursion -}
    Err msg |>> _  = Err msg
    Scalar x |>> _ = Scalar x
    t1@(FiniteTensor index1 (ZipList ts1)) |>> ind
        | Data.List.length (indicesNames t1) > 1 && indexName index1 /= ind =
            FiniteTensor index1 $ ZipList $ (|>> ind) <$> ts1
        | Data.List.length (indicesNames t1) > 1 && indexName index1 == ind =
            let index2 = tensorIndex (Data.List.head ts1)
                dane = (getZipList . tensorData) <$> ts1
                transposed = ZipList <$> Data.List.transpose dane
            in  FiniteTensor index2 $ ZipList $ FiniteTensor index1 <$> transposed
            --(FiniteTensor index1 $ ZipList (head <$> ts1) :  | j <- if isNothing (indexSize index1) then [0 ..] else [0 .. fromJust (indexSize index1) - 1] ]
                --[ts1 !! j |  ]
        | otherwise = t1

