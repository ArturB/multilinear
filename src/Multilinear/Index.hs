{-|
Module      : Index
Description : Implements tensor index.
Copyright   : (c) Artur M. Brodzki, 2018
License     : BSD3
Maintainer  : artur@brodzki.org
Stability   : experimental
Portability : Windows/POSIX

Generic tensor index which may be finitely- or infinitely-dimensional. 

-}

module Multilinear.Index (
    Index(..),
    TIndex(..)
) where

import GHC.Generics

{-| Tensor index class which may be lower (covariant), upper (contravariant) or indifferent. -}
class Index i where

    {-| Index name -}
    indexName :: i -> String

    {-| Returns True if index is lower (covariant), False otherwise. -}
    isCovariant :: i -> Bool

    {-| Returns True if index is upper (contravariant), False otherwise. -}
    isContravariant :: i -> Bool

    {-| Returns True if two indices are equivalent, thus differs only by name, but share same size and type. -}
    equivI :: i -> i -> Bool

    {-| Infix equivalent for 'equiv'. Has low priority equal to 2. -}
    infixl 2 !=!
    (!=!) :: i -> i -> Bool
    i1 !=! i2 = equivI i1 i2

    {-| Convert to generic index type -}
    toTIndex :: i -> TIndex

{-| Generic index type finitely- or infinitely-dimensional -}
data TIndex =
    Covariant {
        indexSize  :: Maybe Int,
        tIndexName :: String
    } |
    Contravariant {
        indexSize  :: Maybe Int,
        tIndexName :: String
    } 
    deriving (Eq, Generic)

{-| Show tensor index -}
instance Show TIndex where
    show (Covariant c n)     = "[" ++ n ++ ":" ++ show c ++ "]"
    show (Contravariant c n) = "<" ++ n ++ ":" ++ show c ++ ">"

{-| Finite index is a Multilinear.Index instance -}
instance Index TIndex where

    {-| Index name -}
    indexName = tIndexName

    {-| Return true if index is covariant |-}
    isCovariant (Covariant _ _) = True
    isCovariant _               = False

    {-| Return true if index is contravariant |-}
    isContravariant (Contravariant _ _) = True
    isContravariant _                   = False

    {-| Returns true if two indices are quivalent, i.e. differs only by name, but share same type and size. -}
    equivI (Covariant count1 _) (Covariant count2 _)
        | count1 == count2 = True
        | otherwise = False
    equivI (Contravariant count1 _) (Contravariant count2 _)
        | count1 == count2 = True
        | otherwise = False
    equivI _ _ = False

    {-| TIndex must not be converted to TIndex -}
    toTIndex = id

{-| Indices can be compared by its name and size |-}
{-| Used to allow to put tensors to typical ordered containers |-}
instance Ord TIndex where
    ind1 <= ind2 = 
        tIndexName ind1 <= tIndexName ind2 || 
        (tIndexName ind1 == tIndexName ind2 && indexSize ind1 <= indexSize ind2)
