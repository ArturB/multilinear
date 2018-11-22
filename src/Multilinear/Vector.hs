{-|
Module      : Multilinear.Vector
Description : Vector constructors (finitely- or infinitely-dimensional)
Copyright   : (c) Artur M. Brodzki, 2018
License     : BSD3
Maintainer  : artur@brodzki.org
Stability   : experimental
Portability : Windows/POSIX

- This module provides convenient constructors that generates a vector (tensor with one upper index) of finite or infinite size. 
- Finitely-dimensional vectors provide much greater performance than infinitely-dimensional 

-}

module Multilinear.Vector (
  -- * Generators
  Multilinear.Vector.fromIndices, 
  Multilinear.Vector.const
) where

import qualified Data.Vector.Unboxed        as Unboxed
import           Multilinear

invalidIndices :: String
invalidIndices = "Indices and its sizes not compatible with structure of vector!"

{-| Generate vector as function of indices -}
fromIndices :: (
    Num a, Unboxed.Unbox a, Multilinear t a
  ) => String     -- ^ Index name (one character)
    -> Int        -- ^ Number of elements
    -> (Int -> a) -- ^ Generator function - returns a vector component at index @i@
    -> t a        -- ^ Generated vector
fromIndices [i] s f = Multilinear.fromIndices  [i] [] [s] [] $ \[x] [] -> f x
fromIndices _ _ _ = error invalidIndices

{-| Generate vector with all components equal to some @v@ -}
const :: (
    Num a, Unboxed.Unbox a, Multilinear t a
  ) => String -- ^ Index name (one character)
    -> Int    -- ^ Number of elements
    -> a      -- ^ Value of each element
    -> t a    -- ^ Generated vector
const [i] s = Multilinear.const  [i] [] [s] []
const _ _ = \_ -> error invalidIndices
