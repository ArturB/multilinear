{-|
Module      : Multilinear.NForm
Description : N-Forms, dot and cross product and determinant (default)
Copyright   : (c) Artur M. Brodzki, 2017
License     : GLP-3
Maintainer  : artur.brodzki@gmail.com
Stability   : experimental
Portability : Windows/POSIX

This module provides convenient constructors that generates n-forms (tensors with n lower indices).

-}

module Multilinear.NForm (
  module X
) where

{-| Default implementation is list -}
import Multilinear.NForm.AsList as X