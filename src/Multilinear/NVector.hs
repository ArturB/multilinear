{-|
Module      : Multilinear.NVector
Description : N-Vectors (default)
Copyright   : (c) Artur M. Brodzki, 2017
License     : GPL-3
Maintainer  : artur.brodzki@gmail.com
Stability   : experimental
Portability : Windows/POSIX

This module provides convenient constructors that generate a n-vector (tensor with n upper indices). 

-}

module Multilinear.NVector (
  module X
) where

{- Default implementation is list -}
import Multilinear.NVector.AsList as X