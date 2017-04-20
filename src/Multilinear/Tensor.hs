{-|
Module      : Tensor
Description : Tensors (default)
Copyright   : (c) Artur M. Brodzki, 2017
License     : GPL-3
Maintainer  : artur.brodzki@gmail.com
Stability   : experimental
Portability : Windows/POSIX

This module provides convenient constructors that generate a arbitrary tensors. 

-}

module Multilinear.Tensor (
  module X
) where

{-| Default implementation is list -}
import Multilinear.Tensor.AsList as X