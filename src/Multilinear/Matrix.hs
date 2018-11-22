{-|
Module      : Multilinear.Matrix
Description : Matrix constructors (finitely- or infinitely dimensional)
Copyright   : (c) Artur M. Brodzki, 2018
License     : BSD3
Maintainer  : artur@brodzki.org
Stability   : experimental
Portability : Windows/POSIX

- This module provides convenient constructors that generates a matrix (finitely- or infinite-dimensional)
- Finitely-dimensional matrices provide much greater performance than infinitely-dimensional

-}

module Multilinear.Matrix (
  -- * Generators
  Multilinear.Matrix.fromIndices, 
  Multilinear.Matrix.const
) where

import qualified Data.Vector.Unboxed        as Unboxed
import           Multilinear

invalidIndices :: String
invalidIndices = "Indices and its sizes not compatible with structure of matrix!"

{-| Generate matrix as function of its indices -}
fromIndices :: (
    Num a, Unboxed.Unbox a, Multilinear t a
  ) => String            -- ^ Indices names (one character per index, first character: rows index, second character: columns index)
    -> Int               -- ^ Number of matrix rows
    -> Int               -- ^ Number of matrix columns
    -> (Int -> Int -> a) -- ^ Generator function - returns a matrix component at @i,j@
    -> t a               -- ^ Generated matrix
fromIndices [u,d] us ds f = Multilinear.fromIndices [u] [d] [us] [ds] $ \[ui] [di] -> f ui di
fromIndices _ _ _ _ = error invalidIndices

{-| Generate matrix with all components equal to @v@ -}
const :: (
    Num a, Unboxed.Unbox a, Multilinear t a
  ) => String -- ^ Indices names (one character per index, first character: rows index, second character: columns index)
    -> Int    -- ^ Number of matrix rows
    -> Int    -- ^ Number of matrix columns
    -> a      -- ^ Value of matrix components
    -> t a    -- ^ Generated matrix
const [u,d] us ds = Multilinear.const [u] [d] [us] [ds]
const _ _ _ = \_ -> error invalidIndices
