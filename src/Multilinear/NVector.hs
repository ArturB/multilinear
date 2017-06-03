{-|
Module      : Multilinear.NVector
Description : N-Vectors constructors (finitely- or infinitely-dimensional)
Copyright   : (c) Artur M. Brodzki, 2017
License     : GPL-3
Maintainer  : artur.brodzki@gmail.com
Stability   : experimental
Portability : Windows/POSIX

- This module provides convenient constructors that generate a n-vector (tensor with n upper indices).  
- Finitely-dimensional n-vectors provide much greater performance than infinitely-dimensional

-}

module Multilinear.NVector (
  -- * Generators
  fromIndices, Multilinear.NVector.const,
  randomDouble, randomDoubleSeed,
  randomInt, randomIntSeed,
) where


import           Control.Monad.Primitive
import           Data.Bits
import qualified Data.Vector                 as Boxed
import           Multilinear.Generic
import           Multilinear.Index.Finite
import           Statistics.Distribution
import qualified System.Random.MWC           as MWC

{-| Generate N-form as function of its indices -}
fromIndices :: (
    Eq a, Show a, Num a, Bits a
  ) => String          -- ^ Indices names (one characted per index)
    -> [Int]           -- ^ Indices sizes
    -> ([Int] -> a)    -- ^ Generator function
    -> Tensor a  -- ^ Generated N-form

fromIndices [] [] f = Scalar $ f []
fromIndices (d:ds) (s:size) f = mergeScalars $ 
    FiniteTensor (Contravariant s [d]) $ Boxed.generate s (\x -> fromIndices ds size (\dss -> f (x:dss)) )
fromIndices _ _ _ = Err "Indices and its sizes incompatible with n-vector structure!"

{-| Generate N-form with all components equal to @v@ -}
const :: (
    Eq a, Show a, Num a, Bits a
  ) => String          -- ^ Indices names (one characted per index)
    -> [Int]           -- ^ Indices sizes
    -> a               -- ^ N-form elements value
    -> Tensor a  -- ^ Generated N-form

const [] [] v = Scalar v
const (d:ds) (s:size) v = mergeScalars $ 
    FiniteTensor (Contravariant s [d]) $ Boxed.replicate (fromIntegral s) $ Multilinear.NVector.const ds size v
const _ _ _ = Err "Indices and its sizes incompatible with n-vector structure!"

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
randomDouble :: (
    ContGen d
  ) => String                    -- ^ Indices names (one character per index)
    -> [Int]                     -- ^ Indices sizes
    -> d                         -- ^ Continuous probability distribution (as from "Statistics.Distribution")
    -> IO (Tensor Double)  -- ^ Generated linear functional

randomDouble [] [] d = do
    component <- MWC.withSystemRandom . MWC.asGenIO $ \gen -> genContVar d gen
    return $ Scalar component

randomDouble (d:ds) (s:size) distr = do
  tensors <- sequence [randomDouble ds size distr | _ <- [0 .. s - 1] ]
  return $  mergeScalars $ FiniteTensor (Contravariant s [d]) $ Boxed.fromList tensors

randomDouble _ _ _ = return $ Err "Indices and its sizes not compatible with structure of n-vector!"

{-| Generate n-vector with random integer components with given probability distribution.
The n-vector is wrapped in the IO monad. -}
{-| Available probability distributions: -}
{-| - Binomial : "Statistics.Distribution.Binomial" -}
{-| - Poisson : "Statistics.Distribution.Poisson" -}
{-| - Geometric : "Statistics.Distribution.Geometric" -}
{-| - Hypergeometric: "Statistics.Distribution.Hypergeometric" -}
randomInt :: (
    DiscreteGen d
  ) => String                -- ^ Indices names (one character per index)
    -> [Int]                   -- ^ Indices sizes
    -> d                     -- ^ Discrete probability distribution (as from "Statistics.Distribution")
    -> IO (Tensor Int)     -- ^ Generated n-vector

randomInt [] [] d = do
    component <- MWC.withSystemRandom . MWC.asGenIO $ \gen -> genDiscreteVar d gen
    return $ Scalar component

randomInt (d:ds) (s:size) distr = do
  tensors <- sequence [randomInt ds size distr | _ <- [0 .. s - 1] ]
  return $  mergeScalars $ FiniteTensor (Contravariant s [d]) $ Boxed.fromList tensors

randomInt _ _ _ = return $ Err "Indices and its sizes not compatible with structure of n-vector!"

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
randomDoubleSeed :: (
    ContGen d, Integral i2, PrimMonad m
  ) => String              -- ^ Index name (one character)
    -> [Int]                 -- ^ Number of elements
    -> d                   -- ^ Continuous probability distribution (as from "Statistics.Distribution")
    -> i2                  -- ^ Randomness seed
    -> m (Tensor Double) -- ^ Generated n-vector

randomDoubleSeed [] [] d seed = do
    gen <- MWC.initialize (Boxed.singleton $ fromIntegral seed)
    component <- genContVar d gen
    return $ Scalar component

randomDoubleSeed (d:ds) (s:size) distr seed = do
  tensors <- sequence [randomDoubleSeed ds size distr seed | _ <- [0 .. s - 1] ]
  return $  mergeScalars $ FiniteTensor (Contravariant s [d]) $ Boxed.fromList tensors

randomDoubleSeed _ _ _ _ = return $ Err "Indices and its sizes not compatible with structure of n-vector!"

{-| Generate n-vector with random integer components with given probability distribution and given seed.
The form is wrapped in a monad. -}
{-| Available probability distributions: -}
{-| - Binomial : "Statistics.Distribution.Binomial" -}
{-| - Poisson : "Statistics.Distribution.Poisson" -}
{-| - Geometric : "Statistics.Distribution.Geometric" -}
{-| - Hypergeometric: "Statistics.Distribution.Hypergeometric" -}
randomIntSeed :: (
    DiscreteGen d, Integral i2, PrimMonad m
  ) => String              -- ^ Index name (one character)
    -> [Int]                 -- ^ Number of elements
    -> d                   -- ^ Discrete probability distribution (as from "Statistics.Distribution")
    -> i2                  -- ^ Randomness seed
    -> m (Tensor Int)    -- ^ Generated n-vector

randomIntSeed [] [] d seed = do
    gen <- MWC.initialize (Boxed.singleton $ fromIntegral seed)
    component <- genDiscreteVar d gen
    return $ Scalar component

randomIntSeed (d:ds) (s:size) distr seed = do
  tensors <- sequence [randomIntSeed ds size distr seed | _ <- [0 .. s - 1] ]
  return $  mergeScalars $ FiniteTensor (Contravariant s [d]) $ Boxed.fromList tensors

randomIntSeed _ _ _ _ = return $ Err "Indices and its sizes not compatible with structure of n-vector!"

