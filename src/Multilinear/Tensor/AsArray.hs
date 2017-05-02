{-|
Module      : Multilinear.Tensor.AsArray
Description : Tensors in array ("Data.Vector") implementation
Copyright   : (c) Artur M. Brodzki, 2017
License     : GPL-3
Maintainer  : artur.brodzki@gmail.com
Stability   : experimental
Portability : Windows/POSIX

- This module provides convenient constructors that generate a arbitrary tensors in array ("Data.Vector") implementation. 
- Choice of container type has great impact on library performance in particular use cases
- Array ("Data.Vector") implementation is generally faster, however it is strict and always keeps all tensor elements in memory, so it may require large amount of RAM.
- List implementation is slower but lazy and when tensor is generated from indices or randomly, it does not generate all elements at once if not necessary,
so it may operate in smaller memory (e.g. linear instead of quadratic when multiplying matrix by vector or form).

-}

{-# LANGUAGE Strict #-}

module Multilinear.Tensor.AsArray (
  -- * Generators
  fromIndices, Multilinear.Tensor.AsArray.const,
  randomDouble, randomDoubleSeed,
  randomInt, randomIntSeed
) where

import           Control.Monad.Primitive
import           Data.Bits
import qualified Data.Vector                as Boxed
import           Multilinear.Generic
import           Multilinear.Generic.AsArray
import           Multilinear.Index.Finite
import           Statistics.Distribution
import qualified System.Random.MWC          as MWC

{-| Generate tensor as functions of its indices -}
fromIndices :: (
    Eq a, Show a, Num a, Bits a
    ) => (String,[Int])          -- ^ Upper indices names (one character per index) and its sizes
      -> (String,[Int])          -- ^ Lower indices names (one character per index) and its sizes
      -> ([Int] -> [Int] -> a)   -- ^ Generator function (f [u1,u2,...] [d1,d2,...] returns a tensor element at t [u1,u2,...] [d1,d2,...])
      -> VectorTensor a          -- ^ Generated tensor

fromIndices ([],[]) ([],[]) f = Scalar $ f [] []
fromIndices (u:us,s:size) d f =
    FiniteTensor (Contravariant s [u]) $ ZipVector $ Boxed.generate s (\x -> fromIndices (us,size) d (\uss dss -> f (x:uss) dss) )
fromIndices u (d:ds,s:size) f =
    FiniteTensor (Covariant s [d]) $ ZipVector $ Boxed.generate s (\x -> fromIndices u (ds,size) (\uss dss -> f uss (x:dss)) )
fromIndices us ds _ = error $ "Indices and its sizes incompatible, upper indices: " ++ show us ++", lower indices: " ++ show ds

{-| Generate tensor with all components equal to @v@ -}
const :: (
    Eq a, Show a, Num a, Bits a
    ) => (String,[Int])     -- ^ Upper indices names (one character per index) and its sizes
      -> (String,[Int])     -- ^ Lower indices names (one character per index) and its sizes
      -> a                  -- ^ Tensor elements value
      -> VectorTensor a     -- ^ Generated tensor

const ([],[]) ([],[]) v = Scalar v
const (u:us,s:size) d v =
    FiniteTensor (Contravariant s [u]) $ ZipVector $ Boxed.replicate (fromIntegral s) $ Multilinear.Tensor.AsArray.const (us,size) d v
const u (d:ds,s:size) v =
    FiniteTensor (    Covariant s [d]) $ ZipVector $ Boxed.replicate (fromIntegral s) $ Multilinear.Tensor.AsArray.const u (ds,size) v
const us ds _ = error $ "Indices and its sizes incompatible, upper indices: " ++ show us ++", lower indices: " ++ show ds

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
randomDouble :: (
    ContGen d
  ) => (String,[Int])          -- ^ Upper indices names (one character per index) and its sizes
    -> (String,[Int])          -- ^ Lower indices names (one character per index) and its sizes
    -> d                     -- ^ Continuous probability distribution (as from "Statistics.Distribution")
    -> IO (VectorTensor Double)  -- ^ Generated tensor

randomDouble ([],[]) ([],[]) distr = do
    component <- MWC.withSystemRandom . MWC.asGenIO $ \gen -> genContVar distr gen
    return $ Scalar component

randomDouble (u:us,s:size) d distr = do
  tensors <- sequence [randomDouble (us,size) d distr | _ <- [0 .. s - 1] ]
  return $ FiniteTensor (Contravariant s [u]) $ ZipVector $ Boxed.fromList tensors

randomDouble u (d:ds,s:size) distr = do
  tensors <- sequence [randomDouble u (ds,size) distr | _ <- [0 .. s - 1] ]
  return $ FiniteTensor (Covariant s [d]) $ ZipVector $ Boxed.fromList tensors

randomDouble us ds _ = 
    return $ Err $ "Indices and its sizes incompatible, upper indices: " ++ show us ++", lower indices: " ++ show ds

{-| Generate tensor with random integer components with given probability distribution.
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
    -> IO (VectorTensor Int)   -- ^ Generated tensor

randomInt ([],[]) ([],[]) distr = do
    component <- MWC.withSystemRandom . MWC.asGenIO $ \gen -> genDiscreteVar distr gen
    return $ Scalar component

randomInt (u:us,s:size) d distr = do
  tensors <- sequence [randomInt (us,size) d distr | _ <- [0 .. s - 1] ]
  return $ FiniteTensor (Contravariant s [u]) $ ZipVector $ Boxed.fromList tensors

randomInt u (d:ds,s:size) distr = do
  tensors <- sequence [randomInt u (ds,size) distr | _ <- [0 .. s - 1] ]
  return $ FiniteTensor (Covariant s [d]) $ ZipVector $ Boxed.fromList tensors

randomInt _ _ _ = return $ Err "Indices and its sizes not compatible with structure of 1-form!"

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
randomDoubleSeed :: (
    ContGen d, PrimMonad m
  ) => (String,[Int])        -- ^ Upper indices names (one character per index) and its sizes
    -> (String,[Int])        -- ^ Lower indices names (one character per index) and its sizes
    -> d                   -- ^ Continuous probability distribution (as from "Statistics.Distribution")
    -> Int                  -- ^ Randomness seed
    -> m (VectorTensor Double) -- ^ Generated tensor

randomDoubleSeed ([],[]) ([],[]) distr seed = do
    gen <- MWC.initialize (Boxed.singleton $ fromIntegral seed)
    component <- genContVar distr gen
    return $ Scalar component

randomDoubleSeed (u:us,s:size) d distr seed = do
  tensors <- sequence [randomDoubleSeed (us,size) d distr seed | _ <- [0 .. s - 1] ]
  return $ FiniteTensor (Contravariant s [u]) $ ZipVector $ Boxed.fromList tensors

randomDoubleSeed u (d:ds,s:size) distr seed = do
  tensors <- sequence [randomDoubleSeed u (ds,size) distr seed | _ <- [0 .. s - 1] ]
  return $ FiniteTensor (Covariant s [d]) $ ZipVector $ Boxed.fromList tensors

randomDoubleSeed _ _ _ _ = return $ Err "Indices and its sizes not compatible with structure of 1-form!"

{-| Generate tensor with random integer components with given probability distribution and given seed.
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
    -> m (VectorTensor Int)    -- ^ Generated tensor

randomIntSeed ([],[]) ([],[]) distr seed = do
    gen <- MWC.initialize (Boxed.singleton $ fromIntegral seed)
    component <- genDiscreteVar distr gen
    return $ Scalar component

randomIntSeed (u:us,s:size) d distr seed = do
  tensors <- sequence [randomIntSeed (us,size) d distr seed | _ <- [0 .. s - 1] ]
  return $ FiniteTensor (Contravariant s [u]) $ ZipVector $ Boxed.fromList tensors

randomIntSeed u (d:ds,s:size) distr seed = do
  tensors <- sequence [randomIntSeed u (ds,size) distr seed | _ <- [0 .. s - 1] ]
  return $ FiniteTensor (Covariant s [d]) $ ZipVector $ Boxed.fromList tensors

randomIntSeed _ _ _ _ = return $ Err "Indices and its sizes not compatible with structure of 1-form!"
