{-|
Module      : Index
Description : Implements tensor index.
Copyright   : (c) Artur M. Brodzki, 2017
License     : GPL-3
Maintainer  : artur.brodzki@gmail.com
Stability   : experimental
Portability : Windows/POSIX

-}

{-# LANGUAGE Strict        #-}

module Multilinear.Index (
    Index(..)
) where

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
