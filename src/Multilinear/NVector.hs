{-|
Module      : Multilinear.NVector
Description : N-Vectors constructors (finitely- or infinitely-dimensional)
Copyright   : (c) Artur M. Brodzki, 2018
License     : BSD3
Maintainer  : artur@brodzki.org
Stability   : experimental
Portability : Windows/POSIX

- This module provides convenient constructors that generate a n-vector (tensor with n upper indices with finite or infinite size).  
- Finitely-dimensional n-vectors provide much greater performance than infinitely-dimensional

-}

module Multilinear.NVector (
  -- * Generators
  Multilinear.NVector.fromIndices, 
  Multilinear.NVector.const
) where

import qualified Data.Vector.Unboxed         as Unboxed
import           Multilinear

{-| Generate n-vector as function of its indices -}
fromIndices :: (
    Num a, Unboxed.Unbox a, Multilinear t a
  ) => String       -- ^ Indices names (one characted per index)
    -> [Int]        -- ^ Indices sizes
    -> ([Int] -> a) -- ^ Generator function
    -> t a          -- ^ Generated n-vector
fromIndices u usize f = Multilinear.fromIndices u [] usize [] $ \uis [] -> f uis

{-| Generate n-vector with all components equal to @v@ -}
const :: (
    Num a, Unboxed.Unbox a, Multilinear t a
  ) => String -- ^ Indices names (one characted per index)
    -> [Int]  -- ^ Indices sizes
    -> a      -- ^ n-vector elements value
    -> t a    -- ^ Generated n-vector
const u usize = Multilinear.const u [] usize []
