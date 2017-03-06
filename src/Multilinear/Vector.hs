{-|
Module      : Vector
Description : Vector is a tensor with only contravariant indices. It refleclts a common understanding of vector as some point in space. 
Copyright   : (c) Artur M. Brodzki, 2017
License     : 3-clause BSD
Maintainer  : artur.brodzki@gmail.com
Stability   : experimental
Portability : Windows/POSIX

-}

module Multilinear.Vector (
  V(..)
) where

{-| Vector datatype -}
data V a where
    V :: String -> ([Int] -> a) -> V a
