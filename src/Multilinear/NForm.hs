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
  Multilinear.NForm.const,
  Multilinear.NForm.randomDouble, 
  Multilinear.NForm.randomDoubleSeed,
  Multilinear.NForm.randomInt, 
  Multilinear.NForm.randomIntSeed,
  -- * Common cases
  Multilinear.NForm.dot, 
  Multilinear.NForm.cross
) where

import           Control.Monad.Primitive
import           Multilinear.Generic
import qualified Multilinear.Tensor       as Tensor
import           Statistics.Distribution

invalidIndices :: String
invalidIndices = "Indices and its sizes incompatible with n-form structure!"

invalidCrossProductIndices :: String
invalidCrossProductIndices = "Indices and its sizes incompatible with cross product structure!"

{-| Generate N-form as function of its indices -}
{-# INLINE fromIndices #-}
fromIndices :: (
    Num a
  ) => String        -- ^ Indices names (one characted per index)
    -> [Int]         -- ^ Indices sizes
    -> ([Int] -> a)  -- ^ Generator function
    -> Tensor a      -- ^ Generated N-form

fromIndices d ds f = Tensor.fromIndices ([],[]) (d,ds) $ \[] -> f

{-| Generate N-form with all components equal to @v@ -}
{-# INLINE Multilinear.NForm.const #-}
const :: (
    Num a
  ) => String    -- ^ Indices names (one characted per index)
    -> [Int]     -- ^ Indices sizes
    -> a         -- ^ N-form elements value
    -> Tensor a  -- ^ Generated N-form

const d ds = Tensor.const ([],[]) (d,ds)

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

randomDouble d ds = Tensor.randomDouble ([],[]) (d,ds)

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

randomInt d ds = Tensor.randomInt ([],[]) (d,ds)

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

randomDoubleSeed d ds = Tensor.randomDoubleSeed ([],[]) (d,ds)

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

randomIntSeed d ds = Tensor.randomIntSeed ([],[]) (d,ds)

{-| 2-form representing a dot product -}
{-# INLINE dot #-}
dot :: (
    Num a
  ) => String    -- ^ Indices names (one characted per index)
    -> Int       -- ^ Size of tensor (dot product is a square tensor)
    -> Tensor a  -- ^ Generated dot product

dot [i1,i2] size = fromIndices [i1,i2] [size,size] (\[i,j] -> if i == j then 1 else 0)
dot _ _ = Err invalidIndices

{-| Tensor representing a cross product (Levi - Civita symbol). It also allows to compute a determinant of square matrix - determinant of matrix @M@ is a equal to length of cross product of all columns of @M@ -}
-- // TODO
{-# INLINE cross #-}
cross :: (
    Num a
  ) => String    -- ^ Indices names (one characted per index)
    -> Int       -- ^ Size of tensor (dot product is a square tensor)
    -> Tensor a  -- ^ Generated dot product

cross [i,j,k] size =
  Tensor.fromIndices ([i],[size]) ([j,k],[size,size])
    (\[_] [_,_] -> 0)
cross _ _ = Err invalidCrossProductIndices
