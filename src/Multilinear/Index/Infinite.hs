{-|
Module      : Index
Description : Infinite-dimensional tensor index.
Copyright   : (c) Artur M. Brodzki, 2017
License     : GPL-3
Maintainer  : artur.brodzki@gmail.com
Stability   : experimental
Portability : Windows/POSIX

Infinite-dimensional tensor index.

-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Strict        #-}

module Multilinear.Index.Infinite (
    Infinite(..)
) where

import           Data.Aeson
import           Data.Hashable
import           Data.Serialize
import           GHC.Generics
import           Multilinear.Index

{-| Index of infinite-dimensional tensor -}
data Infinite =
    Covariant {
        indexName' :: String
    } |
    Contravariant {
        indexName' :: String
    } |
    Indifferent {
        indexName' :: String
    }
    deriving (Eq, Generic)

{-| Show instance of Infinite -}
instance Show Infinite where
    show (Covariant n)     = "[" ++ n ++ "]"
    show (Contravariant n) = "<" ++ n ++ ">"
    show (Indifferent n)   = "(" ++ n ++ ")"

{-| Infinite index is a Multilinear.Index instance -}
instance Index Infinite where

    {-| Index name -}
    indexName = indexName'

    {-| Return true if index is covariant |-}
    isCovariant (Covariant _) = True
    isCovariant _             = False

    {-| Return true if index is contravariant |-}
    isContravariant (Contravariant _) = True
    isContravariant _                 = False

    {-| Return true if index is indifferent |-}
    isIndifferent (Indifferent _) = True
    isIndifferent _               = False

    {-| Returns true if two indices are quivalent, i.e. differs only by name, but share same type. -}
    equivI (Covariant _) (Covariant _)         = True
    equivI (Contravariant _) (Contravariant _) = True
    equivI (Indifferent _) (Indifferent _)     = True
    equivI _ _                                 = False

    {-| Convert to TIndex -}
    toTIndex (Covariant name)     = TCovariant Nothing name
    toTIndex (Contravariant name) = TContravariant Nothing name
    toTIndex (Indifferent name)   = TIndifferent Nothing name

{-| Binary serialization and deserialization |-}
instance Serialize Infinite

{-| Serialization to and from JSON |-}
instance FromJSON Infinite
instance   ToJSON Infinite

{-| Indices can be compared alphabetically by its name |-}
{-| Used to allow to put tensors to typical ordered containers |-}
instance Ord Infinite where
    ind1 <= ind2 = indexName ind1 <= indexName ind2

{-| Indices can be hashed by hash functions |-}
{-| Used to allow to put tensors to typical unordered containers |-}
instance Hashable Infinite

