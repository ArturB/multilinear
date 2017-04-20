{-|
Module      : Vector
Description : Vector (default)
Copyright   : (c) Artur M. Brodzki, 2017
License     : GPL-3
Maintainer  : artur.brodzki@gmail.com
Stability   : experimental
Portability : Windows/POSIX

This module provides convenient constructors that generates a vector (tensor with one upper index).

-}

module Multilinear.Vector (
  module X
) where

{-| Default implementation is list -}
import Multilinear.Vector.AsList as X