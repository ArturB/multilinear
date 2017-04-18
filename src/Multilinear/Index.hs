{-|
Module      : Index
Description : Implements tensor index.
Copyright   : (c) Artur M. Brodzki, 2017
License     : GPL-3
Maintainer  : artur.brodzki@gmail.com
Stability   : experimental
Portability : Windows/POSIX

-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Strict        #-}

module Multilinear.Index (
    Index(..),
    TIndex(..)
) where

import           Data.Aeson
import           Data.Hashable
import           Data.Serialize
import           GHC.Generics

{-| Tensor index which may be lower (covariant), upper (contravariant) or indifferent. -}
class Index i where

    {-| Index name -}
    indexName :: i -> String

    {-| Returns True if index is lower (covariant), False otherwise. -}
    isCovariant :: i -> Bool

    {-| Returns True if index is upper (contravariant), False otherwise. -}
    isContravariant :: i -> Bool

    {-| Returns True if index if indifferent, False otherwise. -}
    isIndifferent :: i -> Bool

    {-| Returns True if two indices are equivalent, thus differs only by name, but share same size and type. -}
    equivI :: i -> i -> Bool

    {-| Infix equivalent for 'equiv'. Has low priority equal to 2. -}
    infixl 2 !=!
    (!=!) :: i -> i -> Bool
    i1 !=! i2 = equivI i1 i2

    {-| Convert to generic index type -}
    toTIndex :: i -> TIndex

{-| Generic index type with Maybe size field -}
data TIndex =
    TCovariant {
        tIndexSize  :: Maybe Int,
        tIndexName :: String
    } |
    TContravariant {
        tIndexSize  :: Maybe Int,
        tIndexName :: String
    } |
    TIndifferent {
        tIndexSize  :: Maybe Int,
        tIndexName :: String
    }
    deriving (Eq, Generic)

{-| Show instance of Finitwe -}
instance Show TIndex where
    show (TCovariant c n)     = "[" ++ n ++ ":" ++ show c ++ "]"
    show (TContravariant c n) = "<" ++ n ++ ":" ++ show c ++ ">"
    show (TIndifferent c n)   = "(" ++ n ++ ":" ++ show c ++ ")"

{-| Finite index is a Multilinear.Index instance -}
instance Index TIndex where

    {-| Index name -}
    indexName = tIndexName

    {-| Return true if index is covariant |-}
    isCovariant (TCovariant _ _) = True
    isCovariant _               = False

    {-| Return true if index is contravariant |-}
    isContravariant (TContravariant _ _) = True
    isContravariant _                   = False

    {-| Return true if index is indifferent |-}
    isIndifferent (TIndifferent _ _) = True
    isIndifferent _                 = False

    {-| Returns true if two indices are quivalent, i.e. differs only by name, but share same type and size. -}
    equivI (TCovariant count1 _) (TCovariant count2 _)
        | count1 == count2 = True
        | otherwise = False
    equivI (TContravariant count1 _) (TContravariant count2 _)
        | count1 == count2 = True
        | otherwise = False
    equivI (TIndifferent count1 _) (TIndifferent count2 _)
        | count1 == count2 = True
        | otherwise = False
    equivI _ _ = False

    {-| TIndex must not be converted to TIndex -}
    toTIndex = id

{-| Binary serialization and deserialization |-}
instance Serialize TIndex

{-| Serialization to and from JSON |-}
instance FromJSON TIndex
instance   ToJSON TIndex

{-| Indices can be compared by its size |-}
{-| Used to allow to put tensors to typical ordered containers |-}
instance Ord TIndex where
    ind1 <= ind2 = tIndexSize ind1 <= tIndexSize ind2

{-| Indices can be hashed by hash functions |-}
{-| Used to allow to put tensors to typical unordered containers |-}
instance Hashable TIndex

