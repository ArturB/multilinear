{-|
Module      : Covector
Description : Implements a Covector datatype - a tensor with only covariant indices, especially 1-covector: linear functional. 
Copyright   : (c) Artur M. Brodzki, 2017
License     : 3-clause BSD
Maintainer  : artur.brodzki@gmail.com
Stability   : experimental
Portability : Windows/POSIX

-}

module Multilinear.Covector (
  T_(..)
) where

{-| Covector datatype -}
data T_ a where
    T_ :: String -> ([Int] -> a) -> T_ a
