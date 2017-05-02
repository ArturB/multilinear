{-|
Module      : Multilinear.Tensor.AsList
Description : Tensors in list implementation
Copyright   : (c) Artur M. Brodzki, 2017
License     : GPL-3
Maintainer  : artur.brodzki@gmail.com
Stability   : experimental
Portability : Windows/POSIX

- This module provides convenient constructors that generate a arbitrary tensors in list implementation. 
- Choice of container type has great impact on library performance in particular use cases
- Array ("Data.Vector") implementation is generally faster, however it is strict and always keeps all tensor elements in memory, so it may require large amount of RAM.
- List implementation is slower but lazy and when tensor is generated from indices or randomly, it does not generate all elements at once if not necessary,
so it may operate in smaller memory (e.g. linear instead of quadratic when multiplying matrix by vector or form).

-}

{-# LANGUAGE Strict #-}

module Multilinear.Tensor.AsList (
  -- * Finite tensors
  fromIndices, Multilinear.Tensor.AsList.const,
  randomDouble, randomDoubleSeed,
  randomInt, randomIntSeed,
  -- * Infinite tensors
  fromIndices', Multilinear.Tensor.AsList.const',
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

-- * FINITE TENSORS

{-| Generate finite tensor as functions of its indices -}
fromIndices :: (
    Eq a, Show a, Num a, Bits a
    ) => (String,[Int])        -- ^ Upper indices names (one character per index) and its sizes
      -> (String,[Int])        -- ^ Lower indices names (one character per index) and its sizes
      -> ([Int] -> [Int] -> a)   -- ^ Generator function (f [u1,u2,...] [d1,d2,...] returns a tensor element at t [u1,u2,...] [d1,d2,...])
      -> ListTensor a          -- ^ Generated tensor

fromIndices ([],[]) ([],[]) f = Scalar $ f [] []
fromIndices (u:us,s:size) d f =
    FiniteTensor (Contravariant (Just s) [u]) $ ZipList [fromIndices (us,size) d (\uss dss -> f (x:uss) dss) | x <- [0 .. s - 1] ]
fromIndices u (d:ds,s:size) f =
    FiniteTensor (Covariant (Just s) [d]) $ ZipList [fromIndices u (ds,size) (\uss dss -> f uss (x:dss)) | x <- [0 .. s - 1] ]
fromIndices us ds _ = error $ "Indices and its sizes incompatible, upper indices: " ++ show us ++", lower indices: " ++ show ds

{-| Generate finite tensor with all components equal to @v@ -}
const :: (
    Eq a, Show a, Num a, Bits a
    ) => (String,[Int])   -- ^ Upper indices names (one character per index) and its sizes
      -> (String,[Int])   -- ^ Lower indices names (one character per index) and its sizes
      -> a              -- ^ Tensor elements value
      -> ListTensor a     -- ^ Generated tensor

const ([],[]) ([],[]) v = Scalar v
const (u:us,s:size) d v =
    FiniteTensor (Contravariant (Just s) [u]) $ ZipList $ replicate (fromIntegral s) $ Multilinear.Tensor.AsList.const (us,size) d v
const u (d:ds,s:size) v =
    FiniteTensor (    Covariant (Just s) [d]) $ ZipList $ replicate (fromIntegral s) $ Multilinear.Tensor.AsList.const u (ds,size) v
const us ds _ = error $ "Indices and its sizes incompatible, upper indices: " ++ show us ++", lower indices: " ++ show ds

{-| Generate finite tensor with random real components with given probability distribution.
The tensor is wrapped in the IO monad. -}
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
  ) => (String,[Int])          -- ^ Upper indices names (one character per index) and its sizes
    -> (String,[Int])          -- ^ Lower indices names (one character per index) and its sizes
    -> d                     -- ^ Continuous probability distribution (as from "Statistics.Distribution")
    -> IO (ListTensor Double)  -- ^ Generated tensor

randomDouble ([],[]) ([],[]) distr = do
    component <- MWC.withSystemRandom . MWC.asGenIO $ \gen -> genContVar distr gen
    return $ Scalar component

randomDouble (u:us,s:size) d distr = do
  tensors <- sequence [randomDouble (us,size) d distr | _ <- [0 .. s - 1] ]
  return $ FiniteTensor (Contravariant (Just s) [u]) $ ZipList tensors

randomDouble u (d:ds,s:size) distr = do
  tensors <- sequence [randomDouble u (ds,size) distr | _ <- [0 .. s - 1] ]
  return $ FiniteTensor (Covariant (Just s) [d]) $ ZipList tensors

randomDouble us ds _ = 
    return $ Err $ "Indices and its sizes incompatible, upper indices: " ++ show us ++", lower indices: " ++ show ds

{-| Generate finite tensor with random integer components with given probability distribution.
The tensor is wrapped in the IO monad. -}
{-| Available probability distributions: -}
{-| - Binomial : "Statistics.Distribution.Binomial" -}
{-| - Poisson : "Statistics.Distribution.Poisson" -}
{-| - Geometric : "Statistics.Distribution.Geometric" -}
{-| - Hypergeometric: "Statistics.Distribution.Hypergeometric" -}
randomInt :: (
    DiscreteGen d
  ) => (String,[Int])        -- ^ Upper indices names (one character per index) and its sizes
    -> (String,[Int])        -- ^ Lower indices names (one character per index) and its sizes
    -> d                   -- ^ Discrete probability distribution (as from "Statistics.Distribution")
    -> IO (ListTensor Int)   -- ^ Generated tensor

randomInt ([],[]) ([],[]) distr = do
    component <- MWC.withSystemRandom . MWC.asGenIO $ \gen -> genDiscreteVar distr gen
    return $ Scalar component

randomInt (u:us,s:size) d distr = do
  tensors <- sequence [randomInt (us,size) d distr | _ <- [0 .. s - 1] ]
  return $ FiniteTensor (Contravariant (Just s) [u]) $ ZipList tensors

randomInt u (d:ds,s:size) distr = do
  tensors <- sequence [randomInt u (ds,size) distr | _ <- [0 .. s - 1] ]
  return $ FiniteTensor (Covariant (Just s) [d]) $ ZipList tensors

randomInt us ds _ = return $ Err $ "Indices and its sizes incompatible, upper indices: " ++ show us ++", lower indices: " ++ show ds

{-| Generate finite tensor with random real components with given probability distribution and given seed.
The tensor is wrapped in a monad. -}
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
    ContGen d, PrimMonad m
  ) => (String,[Int])        -- ^ Upper indices names (one character per index) and its sizes
    -> (String,[Int])        -- ^ Lower indices names (one character per index) and its sizes
    -> d                   -- ^ Continuous probability distribution (as from "Statistics.Distribution")
    -> Int                  -- ^ Randomness seed
    -> m (ListTensor Double) -- ^ Generated tensor

randomDoubleSeed ([],[]) ([],[]) distr seed = do
    gen <- MWC.initialize (Vector.singleton $ fromIntegral seed)
    component <- genContVar distr gen
    return $ Scalar component

randomDoubleSeed (u:us,s:size) d distr seed = do
  tensors <- sequence [randomDoubleSeed (us,size) d distr seed | _ <- [0 .. s - 1] ]
  return $ FiniteTensor (Contravariant (Just s) [u]) $ ZipList tensors

randomDoubleSeed u (d:ds,s:size) distr seed = do
  tensors <- sequence [randomDoubleSeed u (ds,size) distr seed | _ <- [0 .. s - 1] ]
  return $ FiniteTensor (Covariant (Just s) [d]) $ ZipList tensors

randomDoubleSeed us ds _ _ = return $ Err $ "Indices and its sizes incompatible, upper indices: " ++ show us ++", lower indices: " ++ show ds

{-| Generate finite tensor with random integer components with given probability distribution and given seed.
The tensor is wrapped in a monad. -}
{-| Available probability distributions: -}
{-| - Binomial : "Statistics.Distribution.Binomial" -}
{-| - Poisson : "Statistics.Distribution.Poisson" -}
{-| - Geometric : "Statistics.Distribution.Geometric" -}
{-| - Hypergeometric: "Statistics.Distribution.Hypergeometric" -}
randomIntSeed :: (
    DiscreteGen d, PrimMonad m
  ) => (String,[Int])        -- ^ Index name (one character)
    -> (String,[Int])        -- ^ Number of elements
    -> d                   -- ^ Discrete probability distribution (as from "Statistics.Distribution")
    -> Int                  -- ^ Randomness seed
    -> m (ListTensor Int)    -- ^ Generated tensor

randomIntSeed ([],[]) ([],[]) distr seed = do
    gen <- MWC.initialize (Vector.singleton $ fromIntegral seed)
    component <- genDiscreteVar distr gen
    return $ Scalar component

randomIntSeed (u:us,s:size) d distr seed = do
  tensors <- sequence [randomIntSeed (us,size) d distr seed | _ <- [0 .. s - 1] ]
  return $ FiniteTensor (Contravariant (Just s) [u]) $ ZipList tensors

randomIntSeed u (d:ds,s:size) distr seed = do
  tensors <- sequence [randomIntSeed u (ds,size) distr seed | _ <- [0 .. s - 1] ]
  return $ FiniteTensor (Covariant (Just s) [d]) $ ZipList tensors

randomIntSeed us ds _ _ = return $ Err $ "Indices and its sizes incompatible, upper indices: " ++ show us ++", lower indices: " ++ show ds

-- * INFINITE TENSORS

{-| Generate infinite tensor as functions of its indices -}
fromIndices' :: (
    Eq a, Show a, Num a, Bits a
    ) => String                  -- ^ Upper indices names (one character per index)
      -> String                  -- ^ Lower indices names (one character per index)
      -> ([Int] -> [Int] -> a)   -- ^ Generator function (f [u1,u2,...] [d1,d2,...] returns a tensor element at t [u1,u2,...] [d1,d2,...])
      -> ListTensor a            -- ^ Generated tensor

fromIndices' [] [] f = Scalar $ f [] []
fromIndices' (u:us) d f =
    FiniteTensor (Contravariant Nothing [u]) $ ZipList [fromIndices' us d (\uss dss -> f (x:uss) dss) | x <- [0 .. ] ]
fromIndices' u (d:ds) f =
    FiniteTensor (Covariant Nothing [d]) $ ZipList [fromIndices' u ds (\uss dss -> f uss (x:dss)) | x <- [0 .. ] ]

{-| Generate infinite tensor with all components equal to @v@ -}
const' :: (
    Eq a, Show a, Num a, Bits a
    ) => String           -- ^ Upper indices names (one character per index)
      -> String           -- ^ Lower indices names (one character per index)
      -> a                -- ^ Tensor elements value
      -> ListTensor a     -- ^ Generated tensor

const' [] [] v = Scalar v
const' (u:us) d v =
    FiniteTensor (Contravariant Nothing [u]) $ ZipList [Multilinear.Tensor.AsList.const' us d v | _ <- [0 .. ] ]
const' u (d:ds) v =
    FiniteTensor (    Covariant Nothing [d]) $ ZipList [Multilinear.Tensor.AsList.const' u ds v | _ <- [0 .. ] ]

{-| Generate infinite tensor with random real components with given probability distribution.
The tensor is wrapped in the IO monad. -}
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
  ) => String                  -- ^ Upper indices names (one character per index)
    -> String                  -- ^ Lower indices names (one character per index)
    -> d                       -- ^ Continuous probability distribution (as from "Statistics.Distribution")
    -> IO (ListTensor Double)  -- ^ Generated tensor

randomDouble' [] [] distr = do
    component <- MWC.withSystemRandom . MWC.asGenIO $ \gen -> genContVar distr gen
    return $ Scalar component

randomDouble' (u:us) d distr = do
  tensors <- sequence [randomDouble' us d distr | _ <- [0 .. ] ]
  return $ FiniteTensor (Contravariant Nothing [u]) $ ZipList tensors

randomDouble' u (d:ds) distr = do
  tensors <- sequence [randomDouble' u ds distr | _ <- [0 .. ] ]
  return $ FiniteTensor (Covariant Nothing [d]) $ ZipList tensors

{-| Generate infinite tensor with random integer components with given probability distribution.
The tensor is wrapped in the IO monad. -}
{-| Available probability distributions: -}
{-| - Binomial : "Statistics.Distribution.Binomial" -}
{-| - Poisson : "Statistics.Distribution.Poisson" -}
{-| - Geometric : "Statistics.Distribution.Geometric" -}
{-| - Hypergeometric: "Statistics.Distribution.Hypergeometric" -}
randomInt' :: (
    DiscreteGen d
  ) => String                -- ^ Upper indices names (one character per index)
    -> String                -- ^ Lower indices names (one character per index)
    -> d                     -- ^ Discrete probability distribution (as from "Statistics.Distribution")
    -> IO (ListTensor Int)   -- ^ Generated tensor

randomInt' [] [] distr = do
    component <- MWC.withSystemRandom . MWC.asGenIO $ \gen -> genDiscreteVar distr gen
    return $ Scalar component

randomInt' (u:us) d distr = do
  tensors <- sequence [randomInt' us d distr | _ <- [0 .. ] ]
  return $ FiniteTensor (Contravariant Nothing [u]) $ ZipList tensors

randomInt' u (d:ds) distr = do
  tensors <- sequence [randomInt' u ds distr | _ <- [0 .. ] ]
  return $ FiniteTensor (Covariant Nothing [d]) $ ZipList tensors

{-| Generate infinite tensor with random real components with given probability distribution and given seed.
The tensor is wrapped in a monad. -}
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
    ContGen d, PrimMonad m
  ) => String                -- ^ Upper indices names (one character per index)
    -> String                -- ^ Lower indices names (one character per index)
    -> d                     -- ^ Continuous probability distribution (as from "Statistics.Distribution")
    -> Int                   -- ^ Randomness seed
    -> m (ListTensor Double) -- ^ Generated tensor

randomDoubleSeed' [] [] distr seed = do
    gen <- MWC.initialize (Vector.singleton $ fromIntegral seed)
    component <- genContVar distr gen
    return $ Scalar component

randomDoubleSeed' (u:us) d distr seed = do
  tensors <- sequence [randomDoubleSeed' us d distr seed | _ <- [0 .. ] ]
  return $ FiniteTensor (Contravariant Nothing [u]) $ ZipList tensors

randomDoubleSeed' u (d:ds) distr seed = do
  tensors <- sequence [randomDoubleSeed' u ds distr seed | _ <- [0 .. ] ]
  return $ FiniteTensor (Covariant Nothing [d]) $ ZipList tensors

{-| Generate infinite tensor with random integer components with given probability distribution and given seed.
The tensor is wrapped in a monad. -}
{-| Available probability distributions: -}
{-| - Binomial : "Statistics.Distribution.Binomial" -}
{-| - Poisson : "Statistics.Distribution.Poisson" -}
{-| - Geometric : "Statistics.Distribution.Geometric" -}
{-| - Hypergeometric: "Statistics.Distribution.Hypergeometric" -}
randomIntSeed' :: (
    DiscreteGen d, PrimMonad m
  ) => String                -- ^ Upper indices names (one character per index)
    -> String                -- ^ Lower indices names (one character per index)
    -> d                     -- ^ Discrete probability distribution (as from "Statistics.Distribution")
    -> Int                   -- ^ Randomness seed
    -> m (ListTensor Int)    -- ^ Generated tensor

randomIntSeed' [] [] distr seed = do
    gen <- MWC.initialize (Vector.singleton $ fromIntegral seed)
    component <- genDiscreteVar distr gen
    return $ Scalar component

randomIntSeed' (u:us) d distr seed = do
  tensors <- sequence [randomIntSeed' us d distr seed | _ <- [0 .. ] ]
  return $ FiniteTensor (Contravariant Nothing [u]) $ ZipList tensors

randomIntSeed' u (d:ds) distr seed = do
  tensors <- sequence [randomIntSeed' u ds distr seed | _ <- [0 .. ] ]
  return $ FiniteTensor (Covariant Nothing [d]) $ ZipList tensors

