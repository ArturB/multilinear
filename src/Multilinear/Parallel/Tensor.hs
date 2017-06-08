{-|
Module      : Multilinear.Parallel.Tensor
Description : Parallelizable tensors constructors (finitely- or infinitely-dimensional)
Copyright   : (c) Artur M. Brodzki, 2017
License     : GPL-3
Maintainer  : artur.brodzki@gmail.com
Stability   : experimental
Portability : Windows/POSIX

- This module provides convenient constructors that generate a arbitrary, parallelizable tensors of finite or infinite size. 
- Finitely-dimensional tensors provide much greater performance than inifitely-dimensional

-}

module Multilinear.Parallel.Tensor (
  -- * Generators
  fromIndices, Multilinear.Parallel.Tensor.const,
  randomDouble, randomDoubleSeed,
  randomInt, randomIntSeed
) where

import           Control.Monad.Primitive
import qualified Data.Vector                as Boxed
import           Multilinear.Parallel.Generic
import           Multilinear.Index.Finite   as Finite
import           Statistics.Distribution
import qualified System.Random.MWC          as MWC

invalidIndices :: (String, [Int]) -> (String, [Int]) -> String
invalidIndices us ds = "Indices and its sizes incompatible, upper indices: " ++ show us ++", lower indices: " ++ show ds

{-| Generate tensor as functions of its indices -}
{-# INLINE fromIndices #-}
fromIndices :: (
    Num a
    ) => (String,[Int])          -- ^ Upper indices names (one character per index) and its sizes
      -> (String,[Int])          -- ^ Lower indices names (one character per index) and its sizes
      -> ([Int] -> [Int] -> a)   -- ^ Generator function (f [u1,u2,...] [d1,d2,...] returns a tensor element at t [u1,u2,...] [d1,d2,...])
      -> Tensor a                -- ^ Generated tensor

-- If only one upper index is given, generate a SimpleFinite tensor with upper index
fromIndices ([u],[s]) ([],[]) f = 
  SimpleFinite (Contravariant s [u]) $ Boxed.generate s $ \x -> f [x] []

-- If only one lower index is given, generate a SimpleFinite tensor with lower index
fromIndices ([],[]) ([d],[s]) f = 
  SimpleFinite (Covariant s [d]) $ Boxed.generate s $ \x -> f [x] []

-- If many indices are given, first generate upper indices recursively from indices list
fromIndices (u:us,s:size) d f =
    FiniteTensor (Contravariant s [u]) $ Boxed.generate s (\x -> fromIndices (us,size) d (\uss dss -> f (x:uss) dss) )

-- After upper indices, generate lower indices recursively from indices list
fromIndices u (d:ds,s:size) f =
    FiniteTensor (Covariant s [d]) $ Boxed.generate s (\x -> fromIndices u (ds,size) (\uss dss -> f uss (x:dss)) )

-- If there are indices without size or sizes without names, throw an error
fromIndices us ds _ = Err $ invalidIndices us ds

{-| Generate tensor with all components equal to @v@ -}
{-# INLINE Multilinear.Tensor.const #-}
const :: (
    Num a
    ) => (String,[Int]) -- ^ Upper indices names (one character per index) and its sizes
      -> (String,[Int]) -- ^ Lower indices names (one character per index) and its sizes
      -> a              -- ^ Tensor elements value
      -> Tensor a       -- ^ Generated tensor

-- If only one upper index is given, generate a SimpleFinite tensor with upper index
const ([u],[s]) ([],[]) v =
  SimpleFinite (Contravariant s [u]) $ Boxed.replicate s v

-- If only ine lower index is given, generate a SimpleFinite tensor with lower index
const ([],[]) ([d],[s]) v =
  SimpleFinite (Covariant s [d]) $ Boxed.replicate s v

-- If many indices are given, first generate upper indices recursively from indices list
const (u:us,s:size) d v =
    FiniteTensor (Contravariant s [u]) $ Boxed.replicate (fromIntegral s) $ Multilinear.Parallel.Tensor.const (us,size) d v

-- After upper indices, generate lower indices recursively from indices list
const u (d:ds,s:size) v =
    FiniteTensor (Covariant s [d]) $ Boxed.replicate (fromIntegral s) $ Multilinear.Parallel.Tensor.const u (ds,size) v

-- If there are indices without size or sizes without names, throw an error
const us ds _ = Err $ invalidIndices us ds

{-| Generate tensor with random real components with given probability distribution.
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
{-# INLINE randomDouble #-}
randomDouble :: (
    ContGen d
  ) => (String,[Int])      -- ^ Upper indices names (one character per index) and its sizes
    -> (String,[Int])      -- ^ Lower indices names (one character per index) and its sizes
    -> d                   -- ^ Continuous probability distribution (as from "Statistics.Distribution")
    -> IO (Tensor Double)  -- ^ Generated tensor

-- If only one upper index is given, generate a SimpleFinite tensor with upper index
randomDouble ([u],[s]) ([],[]) distr = do
    gen <- MWC.createSystemRandom
    component <- sequence $ Boxed.generate s $ \_ -> genContVar distr gen
    return $ SimpleFinite (Contravariant s [u]) component

-- If only one lower index is given, generate a SimpleFinite tensor with lower index
randomDouble ([],[]) ([d],[s]) distr = do
    gen <- MWC.createSystemRandom
    component <- sequence $ Boxed.generate s $ \_ -> genContVar distr gen
    return $ SimpleFinite (Covariant s [d]) component

-- If many indices are given, first generate upper indices recursively from indices list
randomDouble (u:us,s:size) d distr = do
  tensors <- sequence $ Boxed.generate s $ \_ -> randomDouble (us,size) d distr
  return $ FiniteTensor (Contravariant s [u]) tensors

-- After upper indices, generate lower indices recursively from indices list
randomDouble u (d:ds,s:size) distr = do
  tensors <- sequence $ Boxed.generate s $ \_ -> randomDouble u (ds,size) distr
  return $ FiniteTensor (Covariant s [d]) tensors

-- If there are indices without size or sizes without names, throw an error
randomDouble us ds _ = return $ Err $ invalidIndices us ds

{-| Generate tensor with random integer components with given probability distribution.
The tensor is wrapped in the IO monad. -}
{-| Available probability distributions: -}
{-| - Binomial : "Statistics.Distribution.Binomial" -}
{-| - Poisson : "Statistics.Distribution.Poisson" -}
{-| - Geometric : "Statistics.Distribution.Geometric" -}
{-| - Hypergeometric: "Statistics.Distribution.Hypergeometric" -}
{-# INLINE randomInt #-}
randomInt :: (
    DiscreteGen d
  ) => (String,[Int])    -- ^ Upper indices names (one character per index) and its sizes
    -> (String,[Int])    -- ^ Lower indices names (one character per index) and its sizes
    -> d                 -- ^ Discrete probability distribution (as from "Statistics.Distribution")
    -> IO (Tensor Int)   -- ^ Generated tensor

-- If only one upper index is given, generate a SimpleFinite tensor with upper index
randomInt ([u],[s]) ([],[]) distr = do
    gen <- MWC.createSystemRandom
    component <- sequence $ Boxed.generate s $ \_ -> genDiscreteVar distr gen
    return $ SimpleFinite (Contravariant s [u]) component

-- If only one lower index is given, generate a SimpleFinite tensor with lower index
randomInt ([],[]) ([d],[s]) distr = do
    gen <- MWC.createSystemRandom
    component <- sequence $ Boxed.generate s $ \_ -> genDiscreteVar distr gen
    return $ SimpleFinite (Covariant s [d]) component

-- If many indices are given, first generate upper indices recursively from indices list
randomInt (u:us,s:size) d distr = do
  tensors <- sequence $ Boxed.generate s $ \_ -> randomInt (us,size) d distr
  return $ FiniteTensor (Contravariant s [u]) tensors

-- After upper indices, generate lower indices recursively from indices list
randomInt u (d:ds,s:size) distr = do
  tensors <- sequence $ Boxed.generate s $ \_ -> randomInt u (ds,size) distr
  return $ FiniteTensor (Covariant s [d]) tensors

-- If there are indices without size or sizes without names, throw an error
randomInt us ds _ = return $ Err $ invalidIndices us ds

{-| Generate tensor with random real components with given probability distribution and given seed.
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
{-# INLINE randomDoubleSeed #-}
randomDoubleSeed :: (
    ContGen d, PrimMonad m
  ) => (String,[Int])    -- ^ Upper indices names (one character per index) and its sizes
    -> (String,[Int])    -- ^ Lower indices names (one character per index) and its sizes
    -> d                 -- ^ Continuous probability distribution (as from "Statistics.Distribution")
    -> Int               -- ^ Randomness seed
    -> m (Tensor Double) -- ^ Generated tensor

-- If only one upper index is given, generate a SimpleFinite tensor with upper index
randomDoubleSeed ([u],[s]) ([],[]) distr seed = do
    gen <- MWC.initialize (Boxed.singleton $ fromIntegral seed)
    component <- sequence $ Boxed.generate s $ \_ -> genContVar distr gen
    return $ SimpleFinite (Contravariant s [u]) component

-- If only one lower index is given, generate a SimpleFinite tensor with lower index
randomDoubleSeed ([],[]) ([d],[s]) distr seed = do
    gen <- MWC.initialize (Boxed.singleton $ fromIntegral seed)
    component <- sequence $ Boxed.generate s $ \_ -> genContVar distr gen
    return $ SimpleFinite (Covariant s [d]) component

-- If many indices are given, first generate upper indices recursively from indices list
randomDoubleSeed (u:us,s:size) d distr seed = do
  tensors <- sequence $ Boxed.generate s $ \_ -> randomDoubleSeed (us,size) d distr seed
  return $ FiniteTensor (Contravariant s [u]) tensors

-- After upper indices, generate lower indices recursively from indices list
randomDoubleSeed u (d:ds,s:size) distr seed = do
  tensors <- sequence $ Boxed.generate s $ \_ -> randomDoubleSeed u (ds,size) distr seed
  return $ FiniteTensor (Covariant s [d]) tensors

-- If there are indices without size or sizes without names, throw an error
randomDoubleSeed us ds _ _ = return $ Err $ invalidIndices us ds

{-| Generate tensor with random integer components with given probability distribution and given seed.
The tensor is wrapped in a monad. -}
{-| Available probability distributions: -}
{-| - Binomial : "Statistics.Distribution.Binomial" -}
{-| - Poisson : "Statistics.Distribution.Poisson" -}
{-| - Geometric : "Statistics.Distribution.Geometric" -}
{-| - Hypergeometric: "Statistics.Distribution.Hypergeometric" -}
{-# INLINE randomIntSeed #-}
randomIntSeed :: (
    DiscreteGen d, PrimMonad m
  ) => (String,[Int])    -- ^ Index name (one character)
    -> (String,[Int])    -- ^ Number of elements
    -> d                 -- ^ Discrete probability distribution (as from "Statistics.Distribution")
    -> Int               -- ^ Randomness seed
    -> m (Tensor Int)    -- ^ Generated tensor

-- If only one upper index is given, generate a SimpleFinite tensor with upper index
randomIntSeed ([u],[s]) ([],[]) distr seed = do
    gen <- MWC.initialize (Boxed.singleton $ fromIntegral seed)
    component <- sequence $ Boxed.generate s $ \_ -> genDiscreteVar distr gen
    return $ SimpleFinite (Contravariant s [u]) component

-- If only one lower index is given, generate a SimpleFinite tensor with lower index
randomIntSeed ([],[]) ([d],[s]) distr seed = do
    gen <- MWC.initialize (Boxed.singleton $ fromIntegral seed)
    component <- sequence $ Boxed.generate s $ \_ -> genDiscreteVar distr gen
    return $ SimpleFinite (Covariant s [d]) component

-- If many indices are given, first generate upper indices recursively from indices list
randomIntSeed (u:us,s:size) d distr seed = do
  tensors <- sequence $ Boxed.generate s $ \_ -> randomIntSeed (us,size) d distr seed
  return $ FiniteTensor (Contravariant s [u]) tensors

-- After upper indices, generate lower indices recursively from indices list
randomIntSeed u (d:ds,s:size) distr seed = do
  tensors <- sequence $ Boxed.generate s $ \_ -> randomIntSeed u (ds,size) distr seed
  return $ FiniteTensor (Covariant s [d]) tensors

-- If there are indices without size or sizes without names, throw an error
randomIntSeed us ds _ _ = return $ Err $ invalidIndices us ds
