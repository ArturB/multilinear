{-|
Module      : Multilinear.Tensor
Description : Tensors constructors (finitely- or infinitely-dimensional)
Copyright   : (c) Artur M. Brodzki, 2017
License     : GPL-3
Maintainer  : artur.brodzki@gmail.com
Stability   : experimental
Portability : Windows/POSIX

- This module provides convenient constructors that generate a arbitrary tensors in array ("Data.Vector") implementation. 
- Finitely-dimensional tensors provide much greater performance than inifitely-dimensional

-}

module Multilinear.Tensor (
  -- * Generators
  fromIndices, Multilinear.Tensor.const,
  randomDouble, randomDoubleSeed,
  randomInt, randomIntSeed
) where

import           Control.Monad.Primitive
import           Data.Bits
import qualified Data.Vector                as Boxed
import           Multilinear.Generic
import           Multilinear.Index.Finite
import           Statistics.Distribution
import qualified System.Random.MWC          as MWC

{-| Generate tensor as functions of its indices -}
fromIndices :: (
    Eq a, Show a, Num a, Bits a
    ) => (String,[Int])          -- ^ Upper indices names (one character per index) and its sizes
      -> (String,[Int])          -- ^ Lower indices names (one character per index) and its sizes
      -> ([Int] -> [Int] -> a)   -- ^ Generator function (f [u1,u2,...] [d1,d2,...] returns a tensor element at t [u1,u2,...] [d1,d2,...])
      -> Tensor a          -- ^ Generated tensor

fromIndices ([],[]) ([],[]) f = Scalar $ f [] []
fromIndices (u:us,s:size) d f = mergeScalars $ 
    FiniteTensor (Contravariant s [u]) $ Boxed.generate s (\x -> fromIndices (us,size) d (\uss dss -> f (x:uss) dss) )
fromIndices u (d:ds,s:size) f = mergeScalars $ 
    FiniteTensor (Covariant s [d]) $ Boxed.generate s (\x -> fromIndices u (ds,size) (\uss dss -> f uss (x:dss)) )
fromIndices us ds _ = error $ "Indices and its sizes incompatible, upper indices: " ++ show us ++", lower indices: " ++ show ds

{-| Generate tensor with all components equal to @v@ -}
const :: (
    Eq a, Show a, Num a, Bits a
    ) => (String,[Int])     -- ^ Upper indices names (one character per index) and its sizes
      -> (String,[Int])     -- ^ Lower indices names (one character per index) and its sizes
      -> a                  -- ^ Tensor elements value
      -> Tensor a     -- ^ Generated tensor

const ([],[]) ([],[]) v = Scalar v
const (u:us,s:size) d v = mergeScalars $ 
    FiniteTensor (Contravariant s [u]) $ Boxed.replicate (fromIntegral s) $ Multilinear.Tensor.AsArray.const (us,size) d v
const u (d:ds,s:size) v = mergeScalars $ 
    FiniteTensor (    Covariant s [d]) $ Boxed.replicate (fromIntegral s) $ Multilinear.Tensor.AsArray.const u (ds,size) v
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
    -> IO (Tensor Double)  -- ^ Generated tensor

randomDouble ([],[]) ([],[]) distr = do
    component <- MWC.withSystemRandom . MWC.asGenIO $ \gen -> genContVar distr gen
    return $ Scalar component

randomDouble (u:us,s:size) d distr = do
  tensors <- sequence [randomDouble (us,size) d distr | _ <- [0 .. s - 1] ]
  return $  mergeScalars $ FiniteTensor (Contravariant s [u]) $ Boxed.fromList tensors

randomDouble u (d:ds,s:size) distr = do
  tensors <- sequence [randomDouble u (ds,size) distr | _ <- [0 .. s - 1] ]
  return $  mergeScalars $ FiniteTensor (Covariant s [d]) $ Boxed.fromList tensors

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
    -> IO (Tensor Int)   -- ^ Generated tensor

randomInt ([],[]) ([],[]) distr = do
    component <- MWC.withSystemRandom . MWC.asGenIO $ \gen -> genDiscreteVar distr gen
    return $ Scalar component

randomInt (u:us,s:size) d distr = do
  tensors <- sequence [randomInt (us,size) d distr | _ <- [0 .. s - 1] ]
  return $  mergeScalars $ FiniteTensor (Contravariant s [u]) $ Boxed.fromList tensors

randomInt u (d:ds,s:size) distr = do
  tensors <- sequence [randomInt u (ds,size) distr | _ <- [0 .. s - 1] ]
  return $  mergeScalars $ FiniteTensor (Covariant s [d]) $ Boxed.fromList tensors

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
    -> m (Tensor Double) -- ^ Generated tensor

randomDoubleSeed ([],[]) ([],[]) distr seed = do
    gen <- MWC.initialize (Boxed.singleton $ fromIntegral seed)
    component <- genContVar distr gen
    return $ Scalar component

randomDoubleSeed (u:us,s:size) d distr seed = do
  tensors <- sequence [randomDoubleSeed (us,size) d distr seed | _ <- [0 .. s - 1] ]
  return $  mergeScalars $ FiniteTensor (Contravariant s [u]) $ Boxed.fromList tensors

randomDoubleSeed u (d:ds,s:size) distr seed = do
  tensors <- sequence [randomDoubleSeed u (ds,size) distr seed | _ <- [0 .. s - 1] ]
  return $  mergeScalars $ FiniteTensor (Covariant s [d]) $ Boxed.fromList tensors

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
    -> m (Tensor Int)    -- ^ Generated tensor

randomIntSeed ([],[]) ([],[]) distr seed = do
    gen <- MWC.initialize (Boxed.singleton $ fromIntegral seed)
    component <- genDiscreteVar distr gen
    return $ Scalar component

randomIntSeed (u:us,s:size) d distr seed = do
  tensors <- sequence [randomIntSeed (us,size) d distr seed | _ <- [0 .. s - 1] ]
  return $  mergeScalars $ FiniteTensor (Contravariant s [u]) $ Boxed.fromList tensors

randomIntSeed u (d:ds,s:size) distr seed = do
  tensors <- sequence [randomIntSeed u (ds,size) distr seed | _ <- [0 .. s - 1] ]
  return $  mergeScalars $ FiniteTensor (Covariant s [d]) $ Boxed.fromList tensors

randomIntSeed _ _ _ _ = return $ Err "Indices and its sizes not compatible with structure of 1-form!"
