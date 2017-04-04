{-|
Module      : Multilinear
Description : Provides efficient and generic implementation of linear algebra operation using Ricci - Einstein tensor formalism
Copyright   : (c) Artur M. Brodzki, 2017
License     : 3-clause BSD
Maintainer  : artur.brodzki@gmail.com
Stability   : experimental
Portability : Windows/POSIX

-}

{-# LANGUAGE GADTs  #-}
{-# LANGUAGE Strict  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -O2 #-}

module Multilinear.Generic.AsArray (
    Tensor(..), (!),
    toBinary, toBinaryFile,
    fromBinary, fromBinaryFile,
    Multilinear.Generic.AsArray.toJSON, toJSONFile,
    Multilinear.Generic.AsArray.fromJSON, fromJSONFile
) where

import           GHC.Generics
import           Data.Binary
import           Data.List
import           Data.Hashable
import           Data.Maybe
import           Data.Bits
import           Data.Aeson
import           Codec.Compression.GZip
import qualified Data.ByteString.Lazy as ByteString
import           Multilinear

{- ERROR MESSAGES -}
incompatibleTypes :: String
incompatibleTypes = "Incompatible tensor types!"

{- Tensor defined recursively as scalar or list of other tensors -}
data Tensor i a =
    {-| Tensor may be a scalar -}
    Scalar {
        scalarVal :: a
    } |
    {-| Or a list of other tensors -}
    Tensor {
        tensorIndex :: TIndex i,
        tensorData  :: [Tensor i a]
    } |
    {-| Operations on tensors may throw an error -}
    Err {
        errMessage :: String
    } deriving (Eq, Generic)

-- Recursive indexing
(!) :: (Eq i, Show i, Integral i, Eq a, Show a, Num a) => Tensor i a -> i -> Tensor i a
(!) (Scalar _) _ = Err "Scalar has no indices!"
(!) (Err msg) _ = Err msg
(!) (Tensor _ ts) i = ts !! fromIntegral i

{-| Binary serialization and deserialization -}
instance (Binary i, Binary a) => Binary (Tensor i a)

{-| Serialize to binary string |-}
toBinary :: (Binary i, Binary a) => Tensor i a -> ByteString.ByteString
toBinary = Data.Binary.encode

{-| Write to binary file. Uses compression with gzip |-}
toBinaryFile :: (Binary i, Binary a) => String -> Tensor i a -> IO ()
toBinaryFile name = ByteString.writeFile name . compress . toBinary

{-| Deserialize from binary string |-}
fromBinary :: (Binary i, Binary a) => ByteString.ByteString -> Tensor i a
fromBinary = Data.Binary.decode

{-| Read from binary file |-}
fromBinaryFile :: (Binary i, Binary a) => String -> IO (Tensor i a)
fromBinaryFile name = do
    contents <- ByteString.readFile name
    return $ fromBinary $ decompress contents

{-| Serialization to and from JSON |-}
instance (FromJSON i, FromJSON a) => FromJSON (Tensor i a)
instance (  ToJSON i,   ToJSON a) =>   ToJSON (Tensor i a)

{-| Serialize to JSON string |-}
toJSON :: (ToJSON i, ToJSON a) => Tensor i a -> ByteString.ByteString
toJSON = Data.Aeson.encode

{-| Write to JSON file |-}
toJSONFile :: (ToJSON i, ToJSON a) => String -> Tensor i a -> IO ()
toJSONFile name = ByteString.writeFile name . Multilinear.Generic.AsArray.toJSON

{-| Deserialize from JSON string |-}
fromJSON :: (FromJSON i, FromJSON a) => ByteString.ByteString -> Maybe (Tensor i a)
fromJSON = Data.Aeson.decode

{-| Read from JSON file |-}
fromJSONFile :: (FromJSON i, FromJSON a) => String -> IO (Maybe (Tensor i a))
fromJSONFile name = do
    contents <- ByteString.readFile name
    return $ Multilinear.Generic.AsArray.fromJSON contents

-- Print tensor
instance (Show i, Show a) => Show (Tensor i a) where
    -- merge errors first and then print tensor
    show t = show' $ _mergeErr t
        where 
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

        -- Merge many errors in tensor to the first one
        -- Error tensor is passed further
        _mergeErr (Err msg) = Err msg
        -- Tensor is merged to first error on deeper recursion level
        _mergeErr t1@(Tensor _ ts) =
            -- find first error if present
            let err = Data.List.find _isErrTensor (_mergeErr <$> ts)
            -- and return this error if found, whole tensor otherwise
            in fromMaybe t1 err
        -- in scalars cannot be any error
        _mergeErr (Scalar x) = Scalar x 

        -- return True if tensor is an error
        _isErrTensor :: Tensor i a -> Bool
        _isErrTensor (Err _) = True
        _isErrTensor _       = False

        -- print list elements vertically
        -- used to show contravariant components of tensor, which by convention are written vertically
        _showVertical :: Show a => [a] -> String
        _showVertical v =
            "\n  " ++ ((++) `Data.List.foldl1` vShowed)
            where vShowed = (\x -> "| " ++ show x ++ "\n  ") <$> v :: [String]

-- Tensor is a functor
instance Functor (Tensor i) where
    -- Mapping scalars simply maps its value
    fmap f (Scalar v_) = Scalar (f v_)
    -- Mapping tensors does mapping element by element
    fmap f (Tensor indexT ts) = Tensor indexT v2
        where v2 = fmap (fmap f) ts
    -- Mapping errors changes nothing
    fmap _ (Err msg) = Err msg

-- Tensors can be compared lexigographically
instance (
    Eq i, Ord i, 
    Eq a, Ord a
    ) => Ord (Tensor i a) where

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
    Tensor _ ts1 <= Tensor _ ts2 = ts1 <= ts2

-- You can compute a hash value from tensor
instance (Hashable i, Hashable a) => Hashable (Tensor i a)

-- Tensors can be added, subtracted and multiplicated
instance (
    Eq i, Show i, Integral i, Ord i, Hashable i,
    Eq a, Show a, Num a, Ord a, Hashable a, Bits a
    ) => Num (Tensor i a) where

    -- Adding - element by element
    Scalar x1 + Scalar x2 = Scalar $ x1 + x2
    Scalar x + t = (x+) <$> t
    t + Scalar x = (+x) <$> t
    t1@(Tensor index1 v1) + t2@(Tensor index2 v2)
        | index1 == index2 = Tensor index1 $ Data.List.zipWith (+) v1 v2
        | index1 `Data.List.elem` indices t2 = 
            let t1' = t1 |>>> indexName index1
                t2' = t2 |>>> indexName index1
            in  t1' + t2'
        | otherwise = Tensor index1 [t + t2 | t <- v1]
    Err msg + _ = Err msg
    _ + Err msg = Err msg

    -- Subtracting - element by element
    Scalar x1 - Scalar x2 = Scalar $ x1 - x2
    Scalar x - t = (\e -> x - e) <$> t
    t - Scalar x = (\e -> e - x) <$> t
    t1@(Tensor index1 v1) - t2@(Tensor index2 v2)
        | index1 == index2 = Tensor index1 $ Data.List.zipWith (-) v1 v2
        | index1 `Data.List.elem` indices t2 = 
            let t1' = t1 |>>> indexName index1
                t2' = t2 |>>> indexName index1
            in  t1' - t2'
        | otherwise = Tensor index1 [t - t2 | t <- v1]
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
    t1@(Tensor index1 ts1) * t2@(Tensor index2 _)
        | indexName index1 == indexName index2 = t1 `dot` t2
        | indexName index1 `Data.List.elem` indicesNames t2 =
            let t1' = t1 |>>> indexName index1
                t2' = t2 |>>> indexName index1
            in  t1' * t2'
        | otherwise = Tensor index1 $ (* t2) <$> ts1
        where
        -- Contraction of covariant and contravariant index
        Tensor i1@(Covariant count1 _) ts1' `dot` Tensor i2@(Contravariant count2 _) ts2'
            | count1 == count2 = sum $ Data.List.zipWith (*) ts1' ts2'
            | otherwise = contractionErr i1 i2
        -- Contraction of contravariant and covariant index
        Tensor i1@(Contravariant count1 _) ts1' `dot` Tensor i2@(Covariant count2 _) ts2'
            | count1 == count2 = sum $ Data.List.zipWith (*) ts1' ts2'
            | otherwise = contractionErr i1 i2
        _ `dot` _ = Err "Cannot compute a dot product!"
        contractionErr i1' i2' = Err $
                "Tensor product: " ++ incompatibleTypes ++
                " - index1 is " ++ show i1' ++
                " and index2 is " ++ show i2'
    -- Multiplicating by error simply pushed this error forward
    Err msg * _ = Err msg
    _ * Err msg = Err msg

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

-- Bit operations on tensors
instance (
    Eq i, Show i, Integral i, Ord i, Hashable i, 
    Eq a, Show a, Num a, Ord a, Hashable a, Bits a
    ) => Bits (Tensor i a) where

    -- Bit sum
    Scalar x1 .|. Scalar x2 = Scalar $ x1 .|. x2
    Scalar x .|. t = (x .|.) <$> t
    t .|. Scalar x = (.|. x) <$> t
    t1@(Tensor index1 v1) .|. t2@(Tensor index2 v2)
        | index1 == index2 = Tensor index1 $ Data.List.zipWith (.|.) v1 v2
        | index1 `Data.List.elem` indices t2 = 
            let t1' = t1 |>>> indexName index1
                t2' = t2 |>>> indexName index1
            in  t1' .|. t2'
        | otherwise = Tensor index1 [t .|. t2 | t <- v1]
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
    t1@(Tensor index1 ts1) .&. t2@(Tensor index2 _)
        | indexName index1 == indexName index2 = t1 `dot` t2
        | indexName index1 `Data.List.elem` indicesNames t2 =
            let t1' = t1 |>>> indexName index1
                t2' = t2 |>>> indexName index1
            in  t1' .&. t2'
        | otherwise = Tensor index1 $ (.&. t2) <$> ts1
        where
        -- Contraction of covariant and contravariant index
        Tensor i1@(Covariant count1 _) ts1' `dot` Tensor i2@(Contravariant count2 _) ts2'
            | count1 == count2 = foldl1' (.|.) $ Data.List.zipWith (.&.) ts1' ts2'
            | otherwise = contractionErr i1 i2
        -- Contraction of contravariant and covariant index
        Tensor i1@(Contravariant count1 _) ts1' `dot` Tensor i2@(Covariant count2 _) ts2'
            | count1 == count2 = foldl1' (.|.) $ Data.List.zipWith (.&.) ts1' ts2'
            | otherwise = contractionErr i1 i2
        _ `dot` _ = Err "Cannot compute a bit dot product!"
        contractionErr i1' i2' = Err $
                "Tensor bit product: " ++ incompatibleTypes ++
                " - index1 is " ++ show i1' ++
                " and index2 is " ++ show i2'
    -- Multiplicating by error simply pushed this error forward
    Err msg .&. _ = Err msg
    _ .&. Err msg = Err msg

    -- Bit exclusive sum (XOR)
    Scalar x1 `xor` Scalar x2 = Scalar $ x1 `xor` x2
    Scalar x `xor` t = (x `xor`) <$> t
    t `xor` Scalar x = (`xor` x) <$> t
    t1@(Tensor index1 v1) `xor` t2@(Tensor index2 v2)
        | index1 == index2 = Tensor index1 $ Data.List.zipWith xor v1 v2
        | index1 `Data.List.elem` indices t2 = 
            let t1' = t1 |>>> indexName index1
                t2' = t2 |>>> indexName index1
            in  t1' `xor` t2'
        | otherwise = Tensor index1 [t `xor` t2 | t <- v1]
    Err msg `xor` _ = Err msg
    _ `xor` Err msg = Err msg

    -- Bit complement
    complement = Multilinear.map complement

    -- Bit shift of all elements
    shift t n = Multilinear.map (`shift` n) t

    -- Bit rotating of all elements
    rotate t n = Multilinear.map (`rotate` n) t

    -- Returns number of bits of elements of tensor, -1 for elements of undefined size
    bitSize (Scalar x)       = fromMaybe (-1) $ bitSizeMaybe x
    bitSize (Tensor _ (t:_)) = fromMaybe (-1) $ bitSizeMaybe t
    bitSize (Tensor _ [])    = -1
    bitSize (Err _)          = -1

    -- Returns number of bits of elements of tensor
    bitSizeMaybe (Scalar x)       = bitSizeMaybe x
    bitSizeMaybe (Tensor _ (t:_)) = bitSizeMaybe t
    bitSizeMaybe (Tensor _ [])    = Nothing
    bitSizeMaybe (Err _)          = Nothing

    -- Returns true if tensors element are signed
    isSigned (Scalar x)       = isSigned x
    isSigned (Tensor _ (t:_)) = isSigned t
    isSigned (Tensor _ [])    = False
    isSigned (Err _)          = False

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
    Eq i, Show i, Integral i, Ord i, Hashable i, 
    Eq a, Show a, Fractional a, Ord a, Hashable a, Bits a
    ) => Fractional (Tensor i a) where

    -- Scalar division return result of division of its values
    Scalar x1 / Scalar x2 = Scalar $ x1 / x2
    -- Tensor and scalar are divided value by value
    Scalar x1 / t2 = (x1 /) <$> t2
    t1 / Scalar x2 = (/ x2) <$> t1
    -- Two complex tensors cannot be simply divided
    t1@(Tensor _ _) / t2@(Tensor _ _) = Err $
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
instance (
    Eq i, Show i, Integral i, Ord i, Hashable i, 
    Eq a, Show a, Floating a, Ord a, Hashable a, Bits a
    ) => Floating (Tensor i a) where

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
instance (
    Eq i, Show i, Integral i, Ord i, Hashable i,
    Eq a, Show a, Num a, Ord a, Hashable a, Bits a
    ) => Multilinear Tensor i a where

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

    -- Get tensor order [ (contravariant,covariant)-type ]
    order (Scalar _) = (0,0)
    order (Tensor (Contravariant _ _) t) = (cnvr+1,covr)
        where (cnvr,covr) = order $ Data.List.head t
    order (Tensor (Covariant _ _) t) = (cnvr,covr+1)
        where (cnvr,covr) = order $ Data.List.head t
    order (Tensor (Indifferent _ _) t) = (cnvr,covr)
        where (cnvr,covr) = order $ Data.List.head t
    order (Err _) = (-1,-1)

    -- Get list of all tensor indices
    indices (Scalar _)        = []
    indices (Tensor index ts) = index : indices (Data.List.head ts)
    indices (Err _)           = []    

    -- Rename tensor index
    rename (Scalar x) _ _ = Scalar x
    rename (Tensor i@(Contravariant count name) ts) before after
        | name == before = Tensor (Contravariant count after) $ (\t' -> rename t' before after) <$> ts
        | otherwise = Tensor i $ (\t' -> rename t' before after) <$> ts
    rename (Tensor i@(Covariant count name) ts) before after
        | name == before = Tensor (Covariant count after) $ (\t' -> rename t' before after) <$> ts
        | otherwise = Tensor i $ (\t' -> rename t' before after) <$> ts
    rename (Tensor i@(Indifferent count name) ts) before after
        | name == before = Tensor (Indifferent count after) $ (\t' -> rename t' before after) <$> ts
        | otherwise = Tensor i $ (\t' -> rename t' before after) <$> ts
    rename (Err msg) _ _ = Err msg

    -- Raise an index
    Scalar x /\ _ = Scalar x
    Tensor index ts /\ n
        | indexName index == n = 
            Tensor (Contravariant (indexSize index) n) [t /\ n | t <- ts]
        | otherwise = 
            Tensor index [t /\ n | t <- ts]
    Err msg /\ _ = Err msg

    -- Lower an index
    Scalar x \/ _ = Scalar x
    Tensor index ts \/ n
        | indexName index == n = 
            Tensor (Covariant (indexSize index) n) [t /\ n | t <- ts]
        | otherwise = 
            Tensor index [t /\ n | t <- ts]
    Err msg \/ _ = Err msg

    {-| Transpose a tensor (switch all indices types) |-}
    transpose (Scalar x) = Scalar x
    transpose (Tensor (Covariant count name) ts) =
        Tensor (Contravariant count name) (Multilinear.transpose <$> ts)
    transpose (Tensor (Contravariant count name) ts) =
        Tensor (Covariant count name) (Multilinear.transpose <$> ts)
    transpose (Tensor (Indifferent count name) ts) =
        Tensor (Indifferent count name) (Multilinear.transpose <$> ts)
    transpose (Err msg) = Err msg

    {-| Accessing tensor elements |-}
    el [] t _                      = t
    el _ (Scalar x) _              = Scalar x
    el inds (Tensor index ts) vals = 
        let indval = zip inds vals
            val = find (\(i,_) -> i == indexName index) indval
        in  if isJust val
            then el inds (ts !! fromIntegral (snd $ fromJust val)) vals
            else Tensor index [el inds t vals | t <- ts]
    el _ (Err msg) _               = Err msg

    {-| Mapping with indices. |-}
    iMap f t = iMap' t zeroList
        where 
        zeroList = 0:zeroList

        iMap' (Scalar x) inds        = Scalar $ f inds x
        iMap' (Tensor index ts) inds = Tensor index [iMap' (fst tind) $ inds ++ [snd tind] | tind <- zip ts [0..] ]
        iMap' (Err msg) _            = Err msg
        
    

    {-| Concatenation of two tensor with given index or by creating a new one -}
    augment t1@(Tensor _ _) t2@(Tensor _ _) ind = 
        if indices t1 == indices t2
        then 
            let t1' = t1 <<<| ind
                t2' = t2 <<<| ind
            in  Tensor (tensorIndex t1') $ tensorData t1' ++ tensorData t2'
        else Err "Tensors are not equivalent!"
    augment (Err msg) _ _ = Err msg
    augment _ (Err msg) _ = Err msg
    augment (Scalar _) _ _ = Err "Scalars cannot be augmented!"
    augment _ (Scalar _) _ = Err "Cannot augment by scalar!"

    {-| Shift tensor index right |-}
    {-| Moves given index one level deeper in recursion |-}
    Err msg |>> _  = Err msg
    Scalar x |>> _ = Scalar x
    t1@(Tensor index1 ts1) |>> ind
        | Data.List.length (indices t1) > 1 && indexName index1 /= ind = 
            Tensor index1 [t |>> ind | t <- ts1]
        | Data.List.length (indices t1) > 1 && indexName index1 == ind =
            let index2 = tensorIndex (Data.List.head ts1)
            in Tensor index2 [Tensor index1 [tensorData (ts1 !! fromIntegral j) !! fromIntegral i
                | j <- [0 .. indexSize index1 - 1]]
                | i <- [0 .. indexSize index2 - 1]]
        | otherwise = t1




