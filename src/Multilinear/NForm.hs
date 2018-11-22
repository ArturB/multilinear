{-|
Module      : Multilinear.NForm
Description : N-Forms, dot and cross product and determinant
Copyright   : (c) Artur M. Brodzki, 2018
License     : GLP-3
Maintainer  : artur@brodzki.org
Stability   : experimental
Portability : Windows/POSIX

- This module provides convenient constructors that generates n-forms (tensors with n lower indices with finite or infinite size). 
- Finitely-dimensional n-forms provide much greater performance than infinitely-dimensional

-}

module Multilinear.NForm (
    -- * Generators
  Multilinear.NForm.fromIndices, 
  Multilinear.NForm.const
) where

import qualified Data.Vector.Unboxed      as Unboxed
import           Multilinear

{-| Generate N-form as function of its indices -}
fromIndices :: (
    Num a, Unboxed.Unbox a, Multilinear t a
  ) => String       -- ^ Indices names (one characted per index)
    -> [Int]        -- ^ Indices sizes
    -> ([Int] -> a) -- ^ Generator function
    -> t a          -- ^ Generated N-form
fromIndices d ds f = Multilinear.fromIndices [] d [] ds $ \[] -> f

{-| Generate N-form with all components equal to @v@ -}
const :: (
    Num a, Unboxed.Unbox a, Multilinear t a
  ) => String -- ^ Indices names (one characted per index)
    -> [Int]  -- ^ Indices sizes
    -> a      -- ^ N-form elements value
    -> t a    -- ^ Generated N-form
const d = Multilinear.const [] d []
