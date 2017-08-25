{-|
Module      : Multilinear.NVector
Description : N-Vectors constructors (finitely- or infinitely-dimensional)
Copyright   : (c) Artur M. Brodzki, 2017
License     : GPL-3
Maintainer  : artur.brodzki@gmail.com
Stability   : experimental
Portability : Windows/POSIX

- This module provides convenient constructors that generate a n-vector (tensor with n upper indices with finite or infinite size).  
- Finitely-dimensional n-vectors provide much greater performance than infinitely-dimensional

-}

module Multilinear.NVector (
  -- * Generators
  Multilinear.NVector.fromIndices, 
  Multilinear.NVector.const,
  Multilinear.NVector.randomDouble, 
  Multilinear.NVector.randomDoubleSeed,
  Multilinear.NVector.randomInt, 
  Multilinear.NVector.randomIntSeed,
) where

import           Control.Monad.Primitive
import           Multilinear.Generic
import           Multilinear.Tensor          as Tensor
import           Statistics.Distribution

{-| Generate n-vector as function of its indices -}
{-# INLINE fromIndices #-}
fromIndices :: (
    Num a
  ) => String        -- ^ Indices names (one characted per index)
    -> [Int]         -- ^ Indices sizes
    -> ([Int] -> a)  -- ^ Generator function
    -> Tensor a      -- ^ Generated n-vector

fromIndices u us f = Tensor.fromIndices (u,us) ([],[]) $ \uis [] -> f uis

{-| Generate n-vector with all components equal to @v@ -}
{-# INLINE Multilinear.NForm.const #-}
const :: (
    Num a
  ) => String    -- ^ Indices names (one characted per index)
    -> [Int]     -- ^ Indices sizes
    -> a         -- ^ n-vector elements value
    -> Tensor a  -- ^ Generated n-vector

const u us = Tensor.const (u,us) ([],[])

{-| Generate n-vector with random real components with given probability distribution.
The n-vector is wrapped in the IO monad. -}
{-| Available probability distributions: -}
{-| - Beta : "Statistics.Distribution.BetaDistribution" -}
{-| - Cauchy : "Statistics.Distribution.CauchyLorentz" -}
{-| - Chi-squared : "Statistics.Distribution.ChiSquared" -}
{-| - Exponential : "Statistics.Distribution.Exponential" -}
{-| - Gamma : "Statistics.Distribution.Gamma" -}
{-| - Geometric : "Statistics.Distribution.Geometric" -}
{-| - Normal : "Statistics.Distribution.Normal" -}
{-| - StudentT : "Statistics.Distribution.StudentT" -}
{-| - Uniform : "Statistics.Distribution.Uniform" -}
{-| - F : "Statistics.Distribution.FDistribution" -}
{-| - Laplace : "Statistics.Distribution.Laplace" -}
{-# INLINE randomDouble #-}
randomDouble :: (
    ContGen d
  ) => String              -- ^ Indices names (one character per index)
    -> [Int]               -- ^ Indices sizes
    -> d                   -- ^ Continuous probability distribution (as from "Statistics.Distribution")
    -> IO (Tensor Double)  -- ^ Generated linear functional

randomDouble u us = Tensor.randomDouble (u,us) ([],[])

{-| Generate n-vector with random integer components with given probability distribution.
The n-vector is wrapped in the IO monad. -}
{-| Available probability distributions: -}
{-| - Binomial : "Statistics.Distribution.Binomial" -}
{-| - Poisson : "Statistics.Distribution.Poisson" -}
{-| - Geometric : "Statistics.Distribution.Geometric" -}
{-| - Hypergeometric: "Statistics.Distribution.Hypergeometric" -}
{-# INLINE randomInt #-}
randomInt :: (
    DiscreteGen d
  ) => String              -- ^ Indices names (one character per index)
    -> [Int]               -- ^ Indices sizes
    -> d                   -- ^ Discrete probability distribution (as from "Statistics.Distribution")
    -> IO (Tensor Int)     -- ^ Generated n-vector

randomInt u us = Tensor.randomInt (u,us) ([],[])

{-| Generate n-vector with random real components with given probability distribution and given seed.
The form is wrapped in a monad. -}
{-| Available probability distributions: -}
{-| - Beta : "Statistics.Distribution.BetaDistribution" -}
{-| - Cauchy : "Statistics.Distribution.CauchyLorentz" -}
{-| - Chi-squared : "Statistics.Distribution.ChiSquared" -}
{-| - Exponential : "Statistics.Distribution.Exponential" -}
{-| - Gamma : "Statistics.Distribution.Gamma" -}
{-| - Geometric : "Statistics.Distribution.Geometric" -}
{-| - Normal : "Statistics.Distribution.Normal" -}
{-| - StudentT : "Statistics.Distribution.StudentT" -}
{-| - Uniform : "Statistics.Distribution.Uniform" -}
{-| - F : "Statistics.Distribution.FDistribution" -}
{-| - Laplace : "Statistics.Distribution.Laplace" -}
{-# INLINE randomDoubleSeed #-}
randomDoubleSeed :: (
    ContGen d, PrimMonad m
  ) => String            -- ^ Index name (one character)
    -> [Int]             -- ^ Number of elements
    -> d                 -- ^ Continuous probability distribution (as from "Statistics.Distribution")
    -> Int               -- ^ Randomness seed
    -> m (Tensor Double) -- ^ Generated n-vector

randomDoubleSeed u us = Tensor.randomDoubleSeed (u,us) ([],[])

{-| Generate n-vector with random integer components with given probability distribution and given seed.
The form is wrapped in a monad. -}
{-| Available probability distributions: -}
{-| - Binomial : "Statistics.Distribution.Binomial" -}
{-| - Poisson : "Statistics.Distribution.Poisson" -}
{-| - Geometric : "Statistics.Distribution.Geometric" -}
{-| - Hypergeometric: "Statistics.Distribution.Hypergeometric" -}
{-# INLINE randomIntSeed #-}
randomIntSeed :: (
    DiscreteGen d, PrimMonad m
  ) => String            -- ^ Index name (one character)
    -> [Int]             -- ^ Number of elements
    -> d                 -- ^ Discrete probability distribution (as from "Statistics.Distribution")
    -> Int               -- ^ Randomness seed
    -> m (Tensor Int)    -- ^ Generated n-vector

randomIntSeed u us = Tensor.randomIntSeed (u,us) ([],[])
