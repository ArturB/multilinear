{-|
Module      : Multilinear.Matrix
Description : Matrix (default)
Copyright   : (c) Artur M. Brodzki, 2017
License     : GPL-3
Maintainer  : artur.brodzki@gmail.com
Stability   : experimental
Portability : Windows/POSIX

This module provides convenient constructors that generates a matrix.

-}

module Multilinear.Matrix (
  module X
) where

{- Default implementation is list -}
import Multilinear.Matrix.AsList as X