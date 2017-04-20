{-|
Module      : Multilinear.Form
Description : Linear functional (default)
Copyright   : (c) Artur M. Brodzki, 2017
License     : GPL-3
Maintainer  : artur.brodzki@gmail.com
Stability   : experimental
Portability : Windows/POSIX

This module provides convenient constructors that generates a linear functionals (tensor with one lower index).

-}

module Multilinear.Form (
  module X
) where

{-| Default implementation is list -}
import Multilinear.Form.AsList as X