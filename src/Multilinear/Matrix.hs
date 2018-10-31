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
  Multilinear.Matrix.const,
  Multilinear.Matrix.randomDouble, 
  Multilinear.Matrix.randomDoubleSeed,
  Multilinear.Matrix.randomInt, 
  Multilinear.Matrix.randomIntSeed,
) where

import           Control.Monad.Primitive
import           Multilinear.Generic
import qualified Multilinear.Tensor         as Tensor
import           Statistics.Distribution

invalidIndices :: String
invalidIndices = "Indices and its sizes not compatible with structure of matrix!"

{-| Generate matrix as function of its indices -}
{-# INLINE fromIndices #-}
fromIndices :: (
    Num a
  ) => String               -- ^ Indices names (one character per index, first character: rows index, second character: columns index)
    -> Int                  -- ^ Number of matrix rows
    -> Int                  -- ^ Number of matrix columns
    -> (Int -> Int -> a)    -- ^ Generator function - returns a matrix component at @i,j@
    -> Tensor a             -- ^ Generated matrix

fromIndices [u,d] us ds f = Tensor.fromIndices ([u],[us]) ([d],[ds]) $ \[ui] [di] -> f ui di
fromIndices _ _ _ _ = Err invalidIndices

{-| Generate matrix with all components equal to @v@ -}
{-# INLINE Multilinear.Matrix.const #-}
const :: (
    Num a
  ) => String    -- ^ Indices names (one character per index, first character: rows index, second character: columns index)
    -> Int       -- ^ Number of matrix rows
    -> Int       -- ^ Number of matrix columns
    -> a         -- ^ Value of matrix components
    -> Tensor a  -- ^ Generated matrix

const [u,d] us ds = Tensor.const ([u],[us]) ([d],[ds])
const _ _ _ = \_ -> Err invalidIndices

{-| Generate matrix with random real components with given probability distribution.
The matrix is wrapped in the IO monad. -}
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
  ) => String              -- ^ Indices names (one character per index, first character: rows index, second character: columns index)
    -> Int                 -- ^ Number of matrix rows
    -> Int                 -- ^ Number of matrix columns
    -> d                   -- ^ Continuous probability distribution (as from "Statistics.Distribution")
    -> IO (Tensor Double)  -- ^ Generated matrix

randomDouble [u,d] us ds = Tensor.randomDouble ([u],[us]) ([d],[ds])
randomDouble _ _ _ = \_ -> return $ Err invalidIndices

{-| Generate matrix with random integer components with given probability distribution.
The matrix is wrapped in the IO monad. -}
{-| Available probability distributions: -}
{-| - Binomial : "Statistics.Distribution.Binomial" -}
{-| - Poisson : "Statistics.Distribution.Poisson" -}
{-| - Geometric : "Statistics.Distribution.Geometric" -}
{-| - Hypergeometric: "Statistics.Distribution.Hypergeometric" -}
{-# INLINE randomInt #-}
randomInt :: (
    DiscreteGen d
  ) => String           -- ^ Indices names (one character per index, first character: rows index, second character: columns index)
    -> Int              -- ^ Number of matrix rows
    -> Int              -- ^ Number of matrix columns
    -> d                -- ^ Discrete probability distribution (as from "Statistics.Distribution")
    -> IO (Tensor Int)  -- ^ Generated matrix

randomInt [u,d] us ds = Tensor.randomInt ([u],[us]) ([d],[ds])
randomInt _ _ _ = \_ -> return $ Err invalidIndices

{-| Generate matrix with random real components with given probability distribution and given seed.
The matrix is wrapped in the a monad. -}
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
  ) => String              -- ^ Indices names (one character per index, first character: rows index, second character: columns index)
    -> Int                 -- ^ Number of matrix rows
    -> Int                 -- ^ Number of matrix columns
    -> d                   -- ^ Continuous probability distribution (as from "Statistics.Distribution")
    -> Int                 -- ^ Randomness seed
    -> m (Tensor Double)   -- ^ Generated matrix

randomDoubleSeed [u,d] us ds = Tensor.randomDoubleSeed ([u],[us]) ([d],[ds])
randomDoubleSeed _ _ _ = \_ _ -> return $ Err invalidIndices

{-| Generate matrix with random integer components with given probability distribution. and given seed.
The matrix is wrapped in a monad. -}
{-| Available probability distributions: -}
{-| - Binomial : "Statistics.Distribution.Binomial" -}
{-| - Poisson : "Statistics.Distribution.Poisson" -}
{-| - Geometric : "Statistics.Distribution.Geometric" -}
{-| - Hypergeometric: "Statistics.Distribution.Hypergeometric" -}
{-# INLINE randomIntSeed #-}
randomIntSeed :: (
    DiscreteGen d, PrimMonad m
  ) => String              -- ^ Indices names (one character per index, first character: rows index, second character: columns index)
    -> Int                 -- ^ Number of matrix rows
    -> Int                 -- ^ Number of matrix columns
    -> d                   -- ^ Discrete probability distribution (as from "Statistics.Distribution")
    -> Int                 -- ^ Randomness seed
    -> m (Tensor Int)      -- ^ Generated matrix

randomIntSeed [u,d] us ds = Tensor.randomIntSeed ([u],[us]) ([d],[ds])
randomIntSeed _ _ _ = \_ _ -> return $ Err invalidIndices
