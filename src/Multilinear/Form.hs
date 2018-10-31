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
  -- ** Finite functionals
  Multilinear.Form.fromIndices, 
  Multilinear.Form.const,
  Multilinear.Form.randomDouble,
   Multilinear.Form.randomDoubleSeed,
  Multilinear.Form.randomInt, 
  Multilinear.Form.randomIntSeed,
  -- ** Infinite functionals
  Multilinear.Form.fromIndices', 
  Multilinear.Form.const'
) where

import           Control.Monad.Primitive
import           Multilinear.Generic
import           Multilinear.Index.Infinite as Infinite
import           Multilinear.Tensor         as Tensor
import           Statistics.Distribution

invalidIndices :: String
invalidIndices = "Indices and its sizes not compatible with structure of linear functional!"

-- * Finite functional generators

{-| Generate linear functional as function of indices -}
{-# INLINE fromIndices #-}
fromIndices :: (
    Num a
  ) => String        -- ^ Index name (one character)
    -> Int           -- ^ Number of elements
    -> (Int -> a)    -- ^ Generator function - returns a linear functional component at index @i@
    -> Tensor a      -- ^ Generated linear functional

fromIndices [i] s f = Tensor.fromIndices ([],[]) ([i],[s]) $ \[] [x] -> f x
fromIndices _ _ _ = Err invalidIndices

{-| Generate linear functional with all components equal to some @v@ -}
{-# INLINE Multilinear.Form.const #-}
const :: (
    Num a
  ) => String      -- ^ Index name (one character)
    -> Int         -- ^ Number of elements
    -> a           -- ^ Value of each element
    -> Tensor a    -- ^ Generated linear functional

const [i] s = Tensor.const ([],[]) ([i],[s])
const _ _ = \_ -> Err invalidIndices

{-| Generate linear functional with random real components with given probability distribution.
The functional is wrapped in the IO monad. -}
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
    -> IO (Tensor Double)  -- ^ Generated linear functional

randomDouble [i] s = Tensor.randomDouble ([],[]) ([i],[s])
randomDouble _ _ = \_ -> return $ Err invalidIndices

{-| Generate linear functional with random integer components with given probability distribution.
The functional is wrapped in the IO monad. -}
{-| Available probability distributions: -}
{-| - Binomial : "Statistics.Distribution.Binomial" -}
{-| - Poisson : "Statistics.Distribution.Poisson" -}
{-| - Geometric : "Statistics.Distribution.Geometric" -}
{-| - Hypergeometric: "Statistics.Distribution.Hypergeometric" -}
{-# INLINE randomInt #-}
randomInt :: (
    DiscreteGen d
  ) => String             -- ^ Index name (one character)
    -> Int                -- ^ Number of elements
    -> d                  -- ^ Discrete probability distribution (as from "Statistics.Distribution")
    -> IO (Tensor Int)    -- ^ Generated linear functional

randomInt [i] s = Tensor.randomInt ([],[]) ([i],[s])
randomInt _ _ = \_ -> return $ Err invalidIndices

{-| Generate linear functional with random real components with given probability distribution and given seed.
The functional is wrapped in a monad. -}
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
  ) => String                 -- ^ Index name (one character)
    -> Int                    -- ^ Number of elements
    -> d                      -- ^ Continuous probability distribution (as from "Statistics.Distribution")
    -> Int                    -- ^ Randomness seed
    -> m (Tensor Double)      -- ^ Generated linear functional

randomDoubleSeed [i] s = Tensor.randomDoubleSeed ([],[]) ([i],[s])
randomDoubleSeed _ _ = \_ _ -> return $ Err invalidIndices

{-| Generate linear functional with random integer components with given probability distribution and given seed.
The functional is wrapped in a monad. -}
{-| Available probability distributions: -}
{-| - Binomial : "Statistics.Distribution.Binomial" -}
{-| - Poisson : "Statistics.Distribution.Poisson" -}
{-| - Geometric : "Statistics.Distribution.Geometric" -}
{-| - Hypergeometric: "Statistics.Distribution.Hypergeometric" -}
{-# INLINE randomIntSeed #-}
randomIntSeed :: (
    DiscreteGen d, PrimMonad m
  ) => String                -- ^ Index name (one character)
    -> Int                   -- ^ Number of elements
    -> d                     -- ^ Discrete probability distribution (as from "Statistics.Distribution")
    -> Int                   -- ^ Randomness seed
    -> m (Tensor Int)        -- ^ Generated linear functional

randomIntSeed [i] s = Tensor.randomIntSeed ([],[]) ([i],[s])
randomIntSeed _ _ = \_ _ -> return $ Err invalidIndices

{-| Generate linear functional as function of indices -}
{-# INLINE fromIndices' #-}
fromIndices' :: (
    Num a
  ) => String        -- ^ Index name (one character)
    -> (Int -> a)    -- ^ Generator function - returns a linear functional component at index @i@
    -> Tensor a      -- ^ Generated linear functional

fromIndices' i = case i of
    [d] -> \f -> InfiniteTensor (Infinite.Covariant [d]) $ (Scalar . f) <$> [0..]
    _   -> \_ -> Err invalidIndices

{-| Generate linear functional with all components equal to some @v@ -}
{-# INLINE Multilinear.Form.const' #-}
const' :: (
    Num a
  ) => String      -- ^ Index name (one character)
    -> a           -- ^ Value of each element
    -> Tensor a    -- ^ Generated linear functional

const' i = case i of
    [d] -> \v -> InfiniteTensor (Infinite.Covariant [d]) $ (\_ -> Scalar v) <$> ([0..] :: [Int])
    _   -> \_ -> Err invalidIndices

