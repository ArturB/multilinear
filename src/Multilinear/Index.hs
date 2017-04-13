{-|
Module      : Index
Description : Implements tensor index.
Copyright   : (c) Artur M. Brodzki, 2017
License     : GPL-3
Maintainer  : artur.brodzki@gmail.com
Stability   : experimental
Portability : Windows/POSIX

-}

{-# LANGUAGE Strict #-}
{-# LANGUAGE DeriveGeneric #-}

module Multilinear.Index (
    TIndex(..), 
    isContravariant, 
    isCovariant, 
    isIndifferent,
    equivI, (!=!),
) where

import           Data.Serialize
import           GHC.Generics
import           Data.Hashable
import           Data.Aeson

{-| TENSOR INDEX -}
data TIndex i =
    Covariant {
        indexSize :: i,
        indexName  :: String
    } |
    Contravariant {
        indexSize :: i,
        indexName  :: String
    } |
    Indifferent {
        indexSize :: i,
        indexName  :: String
    }
    deriving (Eq, Generic)

{-| Return true if index is covariant |-}
isCovariant :: TIndex i -> Bool
isCovariant (Covariant _ _) = True
isCovariant _ = False

{-| Return true if index is contravariant |-}
isContravariant :: TIndex i -> Bool
isContravariant (Contravariant _ _) = True
isContravariant _ = False

{-| Return true if index is indifferent |-}
isIndifferent :: TIndex i -> Bool
isIndifferent (Indifferent _ _) = True
isIndifferent _ = False

{-| Binary serialization and deserialization |-}
instance Serialize i => Serialize (TIndex i)

{-| Serialization to and from JSON |-}
instance FromJSON i => FromJSON (TIndex i)
instance   ToJSON i =>   ToJSON (TIndex i)

{-| Show instance of TIndex -}
instance Show i => Show (TIndex i) where
    show (Covariant c n)     = "[" ++ n ++ ":" ++ show c ++ "]"
    show (Contravariant c n) = "<" ++ n ++ ":" ++ show c ++ ">"
    show (Indifferent c n)   = "(" ++ n ++ ":" ++ show c ++ ")"

{-| Indices can be compared by its size |-}
{-| Used to allow to put tensors to typical ordered containers |-}
instance Ord i => Ord (TIndex i) where
    ind1 <= ind2 = indexSize ind1 <= indexSize ind2

{-| Indices can be hashes by hash functions |-}
{-| Used to allow to put tensors to typical unordered containers |-}
instance Hashable i => Hashable (TIndex i)

{-| Returns true if two indices are quivalent, i.e. differs only by name -}
equivI :: Eq i => TIndex i -> TIndex i -> Bool
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

{-| Infix operator equivalent of equiv -}
infixl 6 !=!
(!=!) :: Eq i => TIndex i -> TIndex i -> Bool
(!=!) = equivI


