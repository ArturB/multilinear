{-|
Module      : Multilinear.Form
Description : Linear functional constructors (finitely- or infinitely-dimensional)
Copyright   : (c) Artur M. Brodzki, 2018
License     : BSD3
Maintainer  : artur@brodzki.org
Stability   : experimental
Portability : Windows/POSIX

- This module provides convenient constructors that generates a linear functionals
- Finitely-dimensional functionals provide much greater performance that infinitely-dimensional

-}

module Multilinear.Form (
  -- * Generators
  Multilinear.Form.fromIndices, 
  Multilinear.Form.const
) where

import qualified Data.Vector.Unboxed        as Unboxed
import           Multilinear

invalidIndices :: String
invalidIndices = "Indices and its sizes not compatible with structure of linear functional!"

{-| Generate linear functional as function of indices -}
fromIndices :: (
    Num a, Unboxed.Unbox a, Multilinear t a
  ) => String     -- ^ Index name (one character)
    -> Int        -- ^ Number of elements
    -> (Int -> a) -- ^ Generator function - returns a linear functional component at index @i@
    -> t a        -- ^ Generated linear functional
fromIndices [i] s f = Multilinear.fromIndices [] [i] [] [s] $ \[] [x] -> f x
fromIndices _ _ _ = error invalidIndices

{-| Generate linear functional with all components equal to some @v@ -}
const :: (
    Num a, Unboxed.Unbox a, Multilinear t a
  ) => String -- ^ Index name (one character)
    -> Int    -- ^ Number of elements
    -> a      -- ^ Value of each element
    -> t a    -- ^ Generated linear functional
const [i] s = Multilinear.const [] [i] [] [s]
const _ _ = \_ -> error invalidIndices
