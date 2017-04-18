{-|
Module      : Tensor
Description : Tensors
Copyright   : (c) Artur M. Brodzki, 2017
License     : GPL-3
Maintainer  : artur.brodzki@gmail.com
Stability   : experimental
Portability : Windows/POSIX

This module provides convenient constructors that generate a arbitrary tensors. 

-}

{-# LANGUAGE GADTs  #-}
{-# LANGUAGE Strict #-}

module Multilinear.Tensor (
  fromIndices, Multilinear.Tensor.const,
  randomDouble, randomDoubleSeed,
  randomInt, randomIntSeed
) where

import           Control.Monad.Primitive
import           Data.Bits
import qualified Data.Vector                as Vector
import           Multilinear.Generic.AsList
import           Multilinear.Index.Finite
import           Statistics.Distribution
import qualified System.Random.MWC          as MWC

{-| Generate tensor as functions of its indices -}
fromIndices :: (
    Eq i, Show i, Integral i,
    Eq a, Show a, Num a, Bits a
    ) => (String,[i])        -- ^ Upper indices names (one character per index) and its sizes
      -> (String,[i])        -- ^ Lower indices names (one character per index) and its sizes
      -> ([i] -> [i] -> a)   -- ^ Generator function (f [u1,u2,...] [d1,d2,...] returns a tensor element at t [u1,u2,...] [d1,d2,...])
      -> Tensor i a          -- ^ Generated tensor

fromIndices ([],[]) ([],[]) f = Scalar $ f [] []
fromIndices (u:us,s:size) d f =
    Tensor (Contravariant s [u]) [fromIndices (us,size) d (\uss dss -> f (x:uss) dss) | x <- [0 .. s - 1] ]
fromIndices u (d:ds,s:size) f =
    Tensor (Covariant s [d]) [fromIndices u (ds,size) (\uss dss -> f uss (x:dss)) | x <- [0 .. s - 1] ]
fromIndices us ds _ = error $ "Indices and its sizes incompatible, upper indices: " ++ show us ++", lower indices: " ++ show ds

{-| Generate tensor with all components equal to @v@ -}
const :: (
    Eq i, Show i, Integral i,
    Eq a, Show a, Num a, Bits a
    ) => (String,[i])   -- ^ Upper indices names (one character per index) and its sizes
      -> (String,[i])   -- ^ Lower indices names (one character per index) and its sizes
      -> a              -- ^ Tensor elements value
      -> Tensor i a     -- ^ Generated tensor

const ([],[]) ([],[]) v = Scalar v
const (u:us,s:size) d v =
    Tensor (Contravariant s [u]) $ replicate (fromIntegral s) $ Multilinear.Tensor.const (us,size) d v
const u (d:ds,s:size) v =
    Tensor (    Covariant s [d]) $ replicate (fromIntegral s) $ Multilinear.Tensor.const u (ds,size) v
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
    Eq i, Show i, Integral i,
    ContGen d
  ) => (String,[i])          -- ^ Upper indices names (one character per index) and its sizes
    -> (String,[i])          -- ^ Lower indices names (one character per index) and its sizes
    -> d                     -- ^ Continuous probability distribution (as from "Statistics.Distribution")
    -> IO (Tensor i Double)  -- ^ Generated tensor

randomDouble ([],[]) ([],[]) distr = do
    component <- MWC.withSystemRandom . MWC.asGenIO $ \gen -> genContVar distr gen
    return $ Scalar component

randomDouble (u:us,s:size) d distr = do
  tensors <- sequence [randomDouble (us,size) d distr | _ <- [0 .. s - 1] ]
  return $ Tensor (Contravariant s [u]) tensors

randomDouble u (d:ds,s:size) distr = do
  tensors <- sequence [randomDouble u (ds,size) distr | _ <- [0 .. s - 1] ]
  return $ Tensor (Covariant s [d]) tensors

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
    Eq i, Show i, Integral i,
    DiscreteGen d
  ) => (String,[i])        -- ^ Upper indices names (one character per index) and its sizes
    -> (String,[i])        -- ^ Lower indices names (one character per index) and its sizes
    -> d                   -- ^ Discrete probability distribution (as from "Statistics.Distribution")
    -> IO (Tensor i Int)   -- ^ Generated tensor

randomInt ([],[]) ([],[]) distr = do
    component <- MWC.withSystemRandom . MWC.asGenIO $ \gen -> genDiscreteVar distr gen
    return $ Scalar component

randomInt (u:us,s:size) d distr = do
  tensors <- sequence [randomInt (us,size) d distr | _ <- [0 .. s - 1] ]
  return $ Tensor (Contravariant s [u]) tensors

randomInt u (d:ds,s:size) distr = do
  tensors <- sequence [randomInt u (ds,size) distr | _ <- [0 .. s - 1] ]
  return $ Tensor (Covariant s [d]) tensors

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
    Eq i, Show i, Integral i,
    ContGen d, Integral i2, PrimMonad m
  ) => (String,[i])        -- ^ Upper indices names (one character per index) and its sizes
    -> (String,[i])        -- ^ Lower indices names (one character per index) and its sizes
    -> d                   -- ^ Continuous probability distribution (as from "Statistics.Distribution")
    -> i2                  -- ^ Randomness seed
    -> m (Tensor i Double) -- ^ Generated tensor

randomDoubleSeed ([],[]) ([],[]) distr seed = do
    gen <- MWC.initialize (Vector.singleton $ fromIntegral seed)
    component <- genContVar distr gen
    return $ Scalar component

randomDoubleSeed (u:us,s:size) d distr seed = do
  tensors <- sequence [randomDoubleSeed (us,size) d distr seed | _ <- [0 .. s - 1] ]
  return $ Tensor (Contravariant s [u]) tensors

randomDoubleSeed u (d:ds,s:size) distr seed = do
  tensors <- sequence [randomDoubleSeed u (ds,size) distr seed | _ <- [0 .. s - 1] ]
  return $ Tensor (Covariant s [d]) tensors

randomDoubleSeed _ _ _ _ = return $ Err "Indices and its sizes not compatible with structure of 1-form!"

{-| Generate tensor with random integer components with given probability distribution and given seed.
The tensor is wrapped in a monad. -}
{-| Available probability distributions: -}
{-| - Binomial : "Statistics.Distribution.Binomial" -}
{-| - Poisson : "Statistics.Distribution.Poisson" -}
{-| - Geometric : "Statistics.Distribution.Geometric" -}
{-| - Hypergeometric: "Statistics.Distribution.Hypergeometric" -}
randomIntSeed :: (
    Eq i, Show i, Integral i,
    DiscreteGen d, Integral i2, PrimMonad m
  ) => (String,[i])        -- ^ Index name (one character)
    -> (String,[i])        -- ^ Number of elements
    -> d                   -- ^ Discrete probability distribution (as from "Statistics.Distribution")
    -> i2                  -- ^ Randomness seed
    -> m (Tensor i Int)    -- ^ Generated tensor

randomIntSeed ([],[]) ([],[]) distr seed = do
    gen <- MWC.initialize (Vector.singleton $ fromIntegral seed)
    component <- genDiscreteVar distr gen
    return $ Scalar component

randomIntSeed (u:us,s:size) d distr seed = do
  tensors <- sequence [randomIntSeed (us,size) d distr seed | _ <- [0 .. s - 1] ]
  return $ Tensor (Contravariant s [u]) tensors

randomIntSeed u (d:ds,s:size) distr seed = do
  tensors <- sequence [randomIntSeed u (ds,size) distr seed | _ <- [0 .. s - 1] ]
  return $ Tensor (Covariant s [d]) tensors

randomIntSeed _ _ _ _ = return $ Err "Indices and its sizes not compatible with structure of 1-form!"
