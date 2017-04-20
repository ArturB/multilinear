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

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Strict        #-}

module Multilinear.Index.Finite (
    Finite(..),
) where

import           Data.Aeson
import           Data.Hashable
import           Data.Serialize
import           GHC.Generics
import           Multilinear.Index

{-| Index of finite-dimension tensor with specified size -}
data Finite =
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

{-| Show instance of Finitwe -}
instance Show Finite where
    show (Covariant c n)     = "[" ++ n ++ ":" ++ show c ++ "]"
    show (Contravariant c n) = "<" ++ n ++ ":" ++ show c ++ ">"
    show (Indifferent c n)   = "(" ++ n ++ ":" ++ show c ++ ")"

{-| Finite index is a Multilinear.Index instance -}
instance Index Finite where

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
    toTIndex (Covariant size name)     = TCovariant (Just size) name
    toTIndex (Contravariant size name) = TContravariant (Just size) name
    toTIndex (Indifferent size name)   = TIndifferent (Just size) name

{-| Binary serialization and deserialization |-}
instance Serialize Finite

{-| Serialization to and from JSON |-}
instance FromJSON Finite
instance   ToJSON Finite

{-| Indices can be compared by its size |-}
{-| Used to allow to put tensors to typical ordered containers |-}
instance Ord Finite where
    ind1 <= ind2 = indexSize ind1 <= indexSize ind2

{-| Indices can be hashed by hash functions |-}
{-| Used to allow to put tensors to typical unordered containers |-}
instance Hashable Finite

