{-|
Module      : Multilinear.Parallel.NVector
Description : Parallelizable n-Vectors constructors (finitely- or infinitely-dimensional)
Copyright   : (c) Artur M. Brodzki, 2017
License     : GPL-3
Maintainer  : artur.brodzki@gmail.com
Stability   : experimental
Portability : Windows/POSIX

- This module provides convenient constructors that generate a parallelizable n-vectors (tensors with n upper indices with finite or infinite size).  
- Finitely-dimensional n-vectors provide much greater performance than infinitely-dimensional

-}

module Multilinear.Parallel.NVector (
  -- * Generators
  fromIndices, Multilinear.Parallel.NVector.const,
  randomDouble, randomDoubleSeed,
  randomInt, randomIntSeed,
) where

import           Control.Monad.Primitive
import qualified Data.Vector                 as Boxed
import           Multilinear.Parallel.Generic
import           Multilinear.Index.Finite
import           Statistics.Distribution
import qualified System.Random.MWC           as MWC

invalidIndices :: String
invalidIndices = "Indices and its sizes incompatible with n-vector structure!"

{-| Generate n-vector as function of its indices -}
{-# INLINE fromIndices #-}
fromIndices :: (
    Num a
  ) => String        -- ^ Indices names (one characted per index)
    -> [Int]         -- ^ Indices sizes
    -> ([Int] -> a)  -- ^ Generator function
    -> Tensor a      -- ^ Generated n-vector

fromIndices [d] [s] f = SimpleFinite (Contravariant s [d]) $ Boxed.generate s (\x -> f [x])
fromIndices (d:ds) (s:size) f = 
    FiniteTensor (Contravariant s [d]) $ Boxed.generate s (\x -> fromIndices ds size (\dss -> f (x:dss)) )
fromIndices _ _ _ = Err invalidIndices

{-| Generate n-vector with all components equal to @v@ -}
{-# INLINE Multilinear.NForm.const #-}
const :: (
    Num a
  ) => String    -- ^ Indices names (one characted per index)
    -> [Int]     -- ^ Indices sizes
    -> a         -- ^ n-vector elements value
    -> Tensor a  -- ^ Generated n-vector

const [d] [s] v = SimpleFinite (Contravariant s [d]) $ Boxed.generate s (Prelude.const v)
const (d:ds) (s:size) v = 
    FiniteTensor (Contravariant s [d]) $ Boxed.replicate (fromIntegral s) $ Multilinear.Parallel.NVector.const ds size v
const _ _ _ = Err invalidIndices

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

randomDouble [d] [s] distr = do
    gen <- MWC.createSystemRandom
    components <- sequence $ Boxed.generate s $ \_ -> genContVar distr gen
    return $ SimpleFinite (Contravariant s [d]) components

randomDouble (d:ds) (s:size) distr = do
  tensors <- sequence $ Boxed.generate s $ \_ -> randomDouble ds size distr
  return $ FiniteTensor (Contravariant s [d]) tensors

randomDouble _ _ _ = return $ Err invalidIndices

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

randomInt [d] [s] distr = do
    gen <- MWC.createSystemRandom
    component <- sequence $ Boxed.generate s $ \_ -> genDiscreteVar distr gen
    return $ SimpleFinite (Contravariant s [d]) component

randomInt (d:ds) (s:size) distr = do
  tensors <- sequence $ Boxed.generate s $ \_ -> randomInt ds size distr
  return $ FiniteTensor (Contravariant s [d]) tensors

randomInt _ _ _ = return $ Err invalidIndices

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
    ContGen d, Integral i2, PrimMonad m
  ) => String            -- ^ Index name (one character)
    -> [Int]             -- ^ Number of elements
    -> d                 -- ^ Continuous probability distribution (as from "Statistics.Distribution")
    -> i2                -- ^ Randomness seed
    -> m (Tensor Double) -- ^ Generated n-vector

randomDoubleSeed [d] [s] distr seed = do
    gen <- MWC.initialize (Boxed.singleton $ fromIntegral seed)
    component <- sequence $ Boxed.generate s $ \_ -> genContVar distr gen
    return $ SimpleFinite (Contravariant s [d]) component

randomDoubleSeed (d:ds) (s:size) distr seed = do
  tensors <- sequence $ Boxed.generate s $ \_ -> randomDoubleSeed ds size distr seed
  return $ FiniteTensor (Contravariant s [d]) tensors

randomDoubleSeed _ _ _ _ = return $ Err invalidIndices

{-| Generate n-vector with random integer components with given probability distribution and given seed.
The form is wrapped in a monad. -}
{-| Available probability distributions: -}
{-| - Binomial : "Statistics.Distribution.Binomial" -}
{-| - Poisson : "Statistics.Distribution.Poisson" -}
{-| - Geometric : "Statistics.Distribution.Geometric" -}
{-| - Hypergeometric: "Statistics.Distribution.Hypergeometric" -}
{-# INLINE randomIntSeed #-}
randomIntSeed :: (
    DiscreteGen d, Integral i2, PrimMonad m
  ) => String            -- ^ Index name (one character)
    -> [Int]             -- ^ Number of elements
    -> d                 -- ^ Discrete probability distribution (as from "Statistics.Distribution")
    -> i2                -- ^ Randomness seed
    -> m (Tensor Int)    -- ^ Generated n-vector

randomIntSeed [d] [s] distr seed = do
    gen <- MWC.initialize (Boxed.singleton $ fromIntegral seed)
    component <- sequence $ Boxed.generate s $ \_ -> genDiscreteVar distr gen
    return $ SimpleFinite (Contravariant s [d]) component

randomIntSeed (d:ds) (s:size) distr seed = do
  tensors <- sequence $ Boxed.generate s $ \_ -> randomIntSeed ds size distr seed
  return $ FiniteTensor (Contravariant s [d]) tensors

randomIntSeed _ _ _ _ = return $ Err invalidIndices
