{-|
Module      : Multilinear.NVector.AsList
Description : N-Vectors in list implementation
Copyright   : (c) Artur M. Brodzki, 2017
License     : GPL-3
Maintainer  : artur.brodzki@gmail.com
Stability   : experimental
Portability : Windows/POSIX

- This module provides convenient constructors that generate a n-vector (tensor with n upper indices) in list implementation. 
- Choice of container type has great impact on library performance in particular use cases
- Array ("Data.Vector") implementation is generally faster, however it is strict and always keeps all tensor elements in memory, so it may require large amount of RAM.
- List implementation is slower but lazy and when tensor is generated from indices or randomly, it does not generate all elements at once if not necessary,
so it may operate in smaller memory (e.g. linear instead of quadratic when multiplying matrix by vector or form).

-}

module Multilinear.NVector.AsList (
  -- * Finite N-vectors
  fromIndices, Multilinear.NVector.AsList.const,
  randomDouble, randomDoubleSeed,
  randomInt, randomIntSeed,
  -- * Infinite N-vectors
  fromIndices', Multilinear.NVector.AsList.const',
  randomDouble', randomDoubleSeed',
  randomInt', randomIntSeed'
) where

import           Control.Applicative
import           Control.Monad.Primitive
import           Data.Bits
import qualified Data.Vector                as Vector
import           Multilinear.Generic
import           Multilinear.Generic.AsList
import           Multilinear.Index
import           Statistics.Distribution
import qualified System.Random.MWC          as MWC

-- * FINITE N-VECTORS

{-| Generate finite N-vector as function of its indices -}
fromIndices :: (
    Eq a, Show a, Num a, Bits a
  ) => String        -- ^ Indices names (one characted per index)
    -> [Int]         -- ^ Indices sizes
    -> ([Int] -> a)  -- ^ Generator function
    -> ListTensor a  -- ^ Generated N-vector

fromIndices [] [] f = Scalar $ f []
fromIndices (d:ds) (s:size) f = mergeScalars $ 
    FiniteTensor (Contravariant (Just s) [d]) $ ZipList [fromIndices ds size (\dss -> f (x:dss)) | x <- [0 .. s - 1] ]
fromIndices _ _ _ = Err "Indices and its sizes incompatible with n-vector structure!"

{-| Generate finite N-vector with all components equal to @v@ -}
const :: (
    Eq a, Show a, Num a, Bits a
  ) => String      -- ^ Indices names (one characted per index)
    -> [Int]         -- ^ Indices sizes
    -> a           -- ^ N-vector elements value
    -> ListTensor a  -- ^ Generated N-vector

const [] [] v = Scalar v
const (d:ds) (s:size) v = mergeScalars $ 
    FiniteTensor (Contravariant (Just s) [d]) $ ZipList $ replicate (fromIntegral s) $ Multilinear.NVector.AsList.const ds size v
const _ _ _ = Err "Indices and its sizes incompatible with n-vector structure!"

{-| Generate finite n-vector with random real components with given probability distribution.
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
  ) => String                -- ^ Indices names (one character per index)
    -> [Int]                   -- ^ Indices sizes
    -> d                     -- ^ Continuous probability distribution (as from "Statistics.Distribution")
    -> IO (ListTensor Double)  -- ^ Generated linear functional

randomDouble [] [] d = do
    component <- MWC.withSystemRandom . MWC.asGenIO $ \gen -> genContVar d gen
    return $ Scalar component

randomDouble (d:ds) (s:size) distr = do
  tensors <- sequence [randomDouble ds size distr | _ <- [0 .. s - 1] ]
  return $  mergeScalars $ FiniteTensor (Contravariant (Just s) [d]) $ ZipList tensors

randomDouble _ _ _ = return $ Err "Indices and its sizes not compatible with structure of n-vector!"

{-| Generate finite n-vector with random integer components with given probability distribution.
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
    -> IO (ListTensor Int)     -- ^ Generated n-vector

randomInt [] [] d = do
    component <- MWC.withSystemRandom . MWC.asGenIO $ \gen -> genDiscreteVar d gen
    return $ Scalar component

randomInt (d:ds) (s:size) distr = do
  tensors <- sequence [randomInt ds size distr | _ <- [0 .. s - 1] ]
  return $  mergeScalars $ FiniteTensor (Contravariant (Just s) [d]) $ ZipList tensors

randomInt _ _ _ = return $ Err "Indices and its sizes not compatible with structure of n-vector!"

{-| Generate finite n-vector with random real components with given probability distribution and given seed.
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
    -> m (ListTensor Double) -- ^ Generated n-vector

randomDoubleSeed [] [] d seed = do
    gen <- MWC.initialize (Vector.singleton $ fromIntegral seed)
    component <- genContVar d gen
    return $ Scalar component

randomDoubleSeed (d:ds) (s:size) distr seed = do
  tensors <- sequence [randomDoubleSeed ds size distr seed | _ <- [0 .. s - 1] ]
  return $  mergeScalars $ FiniteTensor (Contravariant (Just s) [d]) $ ZipList tensors

randomDoubleSeed _ _ _ _ = return $ Err "Indices and its sizes not compatible with structure of n-vector!"

{-| Generate finite n-vector with random integer components with given probability distribution and given seed.
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
    -> m (ListTensor Int)    -- ^ Generated n-vector

randomIntSeed [] [] d seed = do
    gen <- MWC.initialize (Vector.singleton $ fromIntegral seed)
    component <- genDiscreteVar d gen
    return $ Scalar component

randomIntSeed (d:ds) (s:size) distr seed = do
  tensors <- sequence [randomIntSeed ds size distr seed | _ <- [0 .. s - 1] ]
  return $  mergeScalars $ FiniteTensor (Contravariant (Just s) [d]) $ ZipList tensors

randomIntSeed _ _ _ _ = return $ Err "Indices and its sizes not compatible with structure of n-vector!"

-- * INFINITE N-VECTORS

{-| Generate infinite n-vector as function of its indices -}
fromIndices' :: (
    Eq a, Show a, Num a, Bits a
  ) => String        -- ^ Indices names (one characted per index)
    -> ([Int] -> a)  -- ^ Generator function
    -> ListTensor a  -- ^ Generated n-vector

fromIndices' [] f = Scalar $ f []
fromIndices' (d:ds) f = mergeScalars $ 
    FiniteTensor (Contravariant Nothing [d]) $ ZipList [fromIndices' ds (\dss -> f (x:dss)) | x <- [0 .. ] ]

{-| Generate infinite n-vector with all components equal to @v@ -}
const' :: (
    Eq a, Show a, Num a, Bits a
  ) => String        -- ^ Indices names (one characted per index)
    -> a             -- ^ n-vector elements value
    -> ListTensor a  -- ^ Generated n-vector

const' [] v = Scalar v
const' (d:ds) v = mergeScalars $ 
    FiniteTensor (Contravariant Nothing [d]) $ ZipList [Multilinear.NVector.AsList.const' ds v | _ <- [0..] ]

{-| Generate infinite n-vector with random real components with given probability distribution.
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
randomDouble' :: (
    ContGen d
  ) => String                  -- ^ Indices names (one character per index)
    -> d                       -- ^ Continuous probability distribution (as from "Statistics.Distribution")
    -> IO (ListTensor Double)  -- ^ Generated linear functional

randomDouble' [] d = do
    component <- MWC.withSystemRandom . MWC.asGenIO $ \gen -> genContVar d gen
    return $ Scalar component

randomDouble' (d:ds) distr = do
  tensors <- sequence [randomDouble' ds distr | _ <- [0 .. ] ]
  return $  mergeScalars $ FiniteTensor (Contravariant Nothing [d]) $ ZipList tensors

{-| Generate infinite n-vector with random integer components with given probability distribution.
The n-vector is wrapped in the IO monad. -}
{-| Available probability distributions: -}
{-| - Binomial : "Statistics.Distribution.Binomial" -}
{-| - Poisson : "Statistics.Distribution.Poisson" -}
{-| - Geometric : "Statistics.Distribution.Geometric" -}
{-| - Hypergeometric: "Statistics.Distribution.Hypergeometric" -}
randomInt' :: (
    DiscreteGen d
  ) => String                  -- ^ Indices names (one character per index)
    -> d                       -- ^ Discrete probability distribution (as from "Statistics.Distribution")
    -> IO (ListTensor Int)     -- ^ Generated n-vector

randomInt' [] d = do
    component <- MWC.withSystemRandom . MWC.asGenIO $ \gen -> genDiscreteVar d gen
    return $ Scalar component

randomInt' (d:ds) distr = do
  tensors <- sequence [randomInt' ds distr | _ <- [0 .. ] ]
  return $  mergeScalars $ FiniteTensor (Contravariant Nothing [d]) $ ZipList tensors

{-| Generate infinite n-vector with random real components with given probability distribution and given seed.
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
randomDoubleSeed' :: (
    ContGen d, Integral i2, PrimMonad m
  ) => String                -- ^ Index name (one character)
    -> d                     -- ^ Continuous probability distribution (as from "Statistics.Distribution")
    -> i2                    -- ^ Randomness seed
    -> m (ListTensor Double) -- ^ Generated n-vector

randomDoubleSeed' [] d seed = do
    gen <- MWC.initialize (Vector.singleton $ fromIntegral seed)
    component <- genContVar d gen
    return $ Scalar component

randomDoubleSeed' (d:ds) distr seed = do
  tensors <- sequence [randomDoubleSeed' ds distr seed | _ <- [0 .. ] ]
  return $  mergeScalars $ FiniteTensor (Contravariant Nothing [d]) $ ZipList tensors

{-| Generate infinite n-vector with random integer components with given probability distribution and given seed.
The form is wrapped in a monad. -}
{-| Available probability distributions: -}
{-| - Binomial : "Statistics.Distribution.Binomial" -}
{-| - Poisson : "Statistics.Distribution.Poisson" -}
{-| - Geometric : "Statistics.Distribution.Geometric" -}
{-| - Hypergeometric: "Statistics.Distribution.Hypergeometric" -}
randomIntSeed' :: (
    DiscreteGen d, Integral i2, PrimMonad m
  ) => String                -- ^ Index name (one character)
    -> d                     -- ^ Discrete probability distribution (as from "Statistics.Distribution")
    -> i2                    -- ^ Randomness seed
    -> m (ListTensor Int)    -- ^ Generated n-vector

randomIntSeed' [] d seed = do
    gen <- MWC.initialize (Vector.singleton $ fromIntegral seed)
    component <- genDiscreteVar d gen
    return $ Scalar component

randomIntSeed' (d:ds) distr seed = do
  tensors <- sequence [randomIntSeed' ds distr seed | _ <- [0 .. ] ]
  return $  mergeScalars $ FiniteTensor (Contravariant Nothing [d]) $ ZipList tensors
