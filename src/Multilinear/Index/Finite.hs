{-|
Module      : Multilinear.Index.Finite
Description : Finite-dimensional tensor index.
Copyright   : (c) Artur M. Brodzki, 2017
License     : GPL-3
Maintainer  : artur.brodzki@gmail.com
Stability   : experimental
Portability : Windows/POSIX

Finite-dimensional tensor index.

-}

module Multilinear.Index.Finite (
    Index(..),
) where

import           Control.DeepSeq
import           Data.Aeson
import           Data.Hashable
import           Data.Serialize
import           GHC.Generics
import qualified Multilinear.Index as TIndex

{-| Index of finite-dimensional tensor with specified size -}
data Index =
    Covariant {
        indexSize  :: Int,
        indexName' :: String
    } |
    Contravariant {
        indexSize  :: Int,
        indexName' :: String
    } |
    Indifferent {
        indexSize  :: Int,
        indexName' :: String
    }
    deriving (Eq, Generic)

{-| Show instance of Finite index -}
instance Show Index where
    show (Covariant c n)     = "[" ++ n ++ ":" ++ show c ++ "]"
    show (Contravariant c n) = "<" ++ n ++ ":" ++ show c ++ ">"
    show (Indifferent c n)   = "(" ++ n ++ ":" ++ show c ++ ")"

{-| Finite index is a Multilinear.Index instance -}
instance TIndex.Index Index where

    {-| Index name -}
    indexName = indexName'

    {-| Return true if index is covariant |-}
    isCovariant (Covariant _ _) = True
    isCovariant _               = False

    {-| Return true if index is contravariant |-}
    isContravariant (Contravariant _ _) = True
    isContravariant _                   = False

    {-| Return true if index is indifferent |-}
    isIndifferent (Indifferent _ _) = True
    isIndifferent _                 = False

    {-| Returns true if two indices are quivalent, i.e. differs only by name, but share same type and size. -}
    equivI (Covariant count1 _) (Covariant count2 _)
        | count1 == count2 = True
        | otherwise = False
    equivI (Contravariant count1 _) (Contravariant count2 _)
        | count1 == count2 = True
        | otherwise = False
    equivI (Indifferent count1 _) (Indifferent count2 _)
        | count1 == count2 = True
        | otherwise = False
    equivI _ _ = False

    {-| Convert to TIndex type -}
    toTIndex (Covariant size name)     = TIndex.Covariant (Just size) name
    toTIndex (Contravariant size name) = TIndex.Contravariant (Just size) name
    toTIndex (Indifferent size name)   = TIndex.Indifferent (Just size) name

{-| Binary serialization and deserialization |-}
instance Serialize Index

{-| Serialization to and from JSON |-}
instance FromJSON Index
instance   ToJSON Index

{-| NFData instance -}
instance NFData Index

{-| Indices can be compared by its size |-}
{-| Used to allow to put tensors to typical ordered containers |-}
instance Ord Index where
    ind1 <= ind2 = indexSize ind1 <= indexSize ind2

{-| Indices can be hashed by hash functions |-}
{-| Used to allow to put tensors to typical unordered containers |-}
instance Hashable Index

