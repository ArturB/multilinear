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
  Multilinear.Vector.const,
  Multilinear.Vector.randomDouble, 
  Multilinear.Vector.randomDoubleSeed,
  Multilinear.Vector.randomInt, 
  Multilinear.Vector.randomIntSeed
) where

import           Control.Monad.Primitive
import           Multilinear.Generic
import           Multilinear.Tensor         as Tensor
import           Statistics.Distribution

invalidIndices :: String
invalidIndices = "Indices and its sizes not compatible with structure of vector!"

{-| Generate vector as function of indices -}
{-# INLINE fromIndices #-}
fromIndices :: (
    Num a
  ) => String        -- ^ Index name (one character)
    -> Int           -- ^ Number of elements
    -> (Int -> a)    -- ^ Generator function - returns a vector component at index @i@
    -> Tensor a      -- ^ Generated vector

fromIndices [i] s f = Tensor.fromIndices ([i],[s]) ([],[]) $ \[x] [] -> f x
fromIndices _ _ _ = Err invalidIndices

{-| Generate vector with all components equal to some @v@ -}
{-# INLINE Multilinear.Vector.const #-}
const :: (
    Num a
  ) => String      -- ^ Index name (one character)
    -> Int         -- ^ Number of elements
    -> a           -- ^ Value of each element
    -> Tensor a    -- ^ Generated vector

const [i] s = Tensor.const ([i],[s]) ([],[])
const _ _ = \_ -> Err invalidIndices

{-| Generate vector with random real components with given probability distribution.
The vector is wrapped in the IO monad. -}
{-| Available probability distributions: -}
{-| - Beta : "Statistics.Distribution.BetaDistribution" -}
{-| - Cauchy : "Statistics.Distribution.CauchyLorentz" -}
{-| - Chi-squared : "Statistics.Distribution.ChiSquared" -}
{-| - Exponential : "Statistics.Distribution.Exponential" -}
{-| - Gamma : "Statistics.Distribution.Gamma" -}
{-| - Normal : "Statistics.Distribution.Normal" -}
{-| - StudentT : "Statistics.Distribution.StudentT" -}
{-| - Uniform : "Statistics.Distribution.Uniform" -}
{-| - F : "Statistics.Distribution.FDistribution" -}
{-| - Laplace : "Statistics.Distribution.Laplace" -}
{-# INLINE randomDouble #-}
randomDouble :: (
    ContGen d
  ) => String              -- ^ Index name (one character)
    -> Int                 -- ^ Number of elements
    -> d                   -- ^ Continuous probability distribution (as from "Statistics.Distribution")
    -> IO (Tensor Double)  -- ^ Generated vector

randomDouble [i] s = Tensor.randomDouble ([i],[s]) ([],[])
randomDouble _ _ = \_ -> return $ Err invalidIndices

{-| Generate vector with random integer components with given probability distribution.
The vector is wrapped in the IO monad. -}
{-| Available probability distributions: -}
{-| - Binomial : "Statistics.Distribution.Binomial" -}
{-| - Poisson : "Statistics.Distribution.Poisson" -}
{-| - Geometric : "Statistics.Distribution.Geometric" -}
{-| - Hypergeometric: "Statistics.Distribution.Hypergeometric" -}
{-# INLINE randomInt #-}
randomInt :: (
    DiscreteGen d
  ) => String           -- ^ Index name (one character)
    -> Int              -- ^ Number of elements
    -> d                -- ^ Discrete probability distribution (as from "Statistics.Distribution")
    -> IO (Tensor Int)  -- ^ Generated vector

randomInt [i] s = Tensor.randomInt ([i],[s]) ([],[])
randomInt _ _ = \_ -> return $ Err invalidIndices

{-| Generate vector with random real components with given probability distribution and given seed.
The vector is wrapped in a monad. -}
{-| Available probability distributions: -}
{-| - Beta : "Statistics.Distribution.BetaDistribution" -}
{-| - Cauchy : "Statistics.Distribution.CauchyLorentz" -}
{-| - Chi-squared : "Statistics.Distribution.ChiSquared" -}
{-| - Exponential : "Statistics.Distribution.Exponential" -}
{-| - Gamma : "Statistics.Distribution.Gamma" -}
{-| - Normal : "Statistics.Distribution.Normal" -}
{-| - StudentT : "Statistics.Distribution.StudentT" -}
{-| - Uniform : "Statistics.Distribution.Uniform" -}
{-| - F : "Statistics.Distribution.FDistribution" -}
{-| - Laplace : "Statistics.Distribution.Laplace" -}
{-# INLINE randomDoubleSeed #-}
randomDoubleSeed :: (
    ContGen d, PrimMonad m
  ) => String             -- ^ Index name (one character)
    -> Int                -- ^ Number of elements
    -> d                  -- ^ Continuous probability distribution (as from "Statistics.Distribution")
    -> Int                -- ^ Randomness seed
    -> m (Tensor Double)  -- ^ Generated vector

randomDoubleSeed [i] s = Tensor.randomDoubleSeed ([i],[s]) ([],[])
randomDoubleSeed _ _ = \_ _ -> return $ Err invalidIndices

{-| Generate vector with random integer components with given probability distribution and given seed.
The vector is wrapped in a monad. -}
{-| Available probability distributions: -}
{-| - Binomial : "Statistics.Distribution.Binomial" -}
{-| - Poisson : "Statistics.Distribution.Poisson" -}
{-| - Geometric : "Statistics.Distribution.Geometric" -}
{-| - Hypergeometric: "Statistics.Distribution.Hypergeometric" -}
{-# INLINE randomIntSeed #-}
randomIntSeed :: (
    DiscreteGen d, PrimMonad m
  ) => String          -- ^ Index name (one character)
    -> Int             -- ^ Number of elements
    -> d               -- ^ Discrete probability distribution (as from "Statistics.Distribution")
    -> Int             -- ^ Randomness seed
    -> m (Tensor Int)  -- ^ Generated vector

randomIntSeed [i] s = Tensor.randomIntSeed ([i],[s]) ([],[])
randomIntSeed _ _ = \_ _ -> return $ Err invalidIndices

