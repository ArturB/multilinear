{-|
Module      : Multilinear.Parallel.NForm
Description : Parallelizable N-Forms, dot and cross product and determinant
Copyright   : (c) Artur M. Brodzki, 2017
License     : GLP-3
Maintainer  : artur.brodzki@gmail.com
Stability   : experimental
Portability : Windows/POSIX

- This module provides convenient constructors that generates parallelizable n-forms (tensors with n lower indices of finite or infinite size).
- Finitely-dimensional n-forms provide much greater performance than infinitely-dimensional

-}

module Multilinear.Parallel.NForm (
    -- * Generators
  fromIndices, Multilinear.Parallel.NForm.const,
  randomDouble, randomDoubleSeed,
  randomInt, randomIntSeed,
  -- * Common cases
  Multilinear.Parallel.NForm.dot, cross
) where

import           Control.Monad.Primitive
import qualified Data.Vector                  as Boxed
import           Multilinear.Index.Finite
import           Multilinear.Parallel.Generic
import qualified Multilinear.Parallel.Tensor  as Tensor
import           Statistics.Distribution
import qualified System.Random.MWC            as MWC

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

fromIndices [d] [s] f = SimpleFinite (Covariant s [d]) $ Boxed.generate s (\x -> f [x])
fromIndices (d:ds) (s:size) f =
    FiniteTensor (Covariant s [d]) $ Boxed.generate s (\x -> fromIndices ds size (\dss -> f (x:dss)) )
fromIndices _ _ _ = Err invalidIndices

{-| Generate N-form with all components equal to @v@ -}
{-# INLINE Multilinear.NForm.const #-}
const :: (
    Num a
  ) => String    -- ^ Indices names (one characted per index)
    -> [Int]     -- ^ Indices sizes
    -> a         -- ^ N-form elements value
    -> Tensor a  -- ^ Generated N-form

const [d] [s] v = SimpleFinite (Contravariant s [d]) $ Boxed.generate s (Prelude.const v)
const (d:ds) (s:size) v =
    FiniteTensor (Covariant s [d]) $ Boxed.replicate (fromIntegral s) $ Multilinear.Parallel.NForm.const ds size v
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
    return $ SimpleFinite (Covariant s [d]) components

randomDouble (d:ds) (s:size) distr = do
  tensors <- sequence $ Boxed.generate s $ \_ -> randomDouble ds size distr
  return $ FiniteTensor (Covariant s [d]) tensors

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
    return $ SimpleFinite (Covariant s [d]) component

randomInt (d:ds) (s:size) distr = do
  tensors <- sequence $ Boxed.generate s $ \_ -> randomInt ds size distr
  return $ FiniteTensor (Covariant s [d]) tensors

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
    return $ SimpleFinite (Covariant s [d]) component

randomDoubleSeed (d:ds) (s:size) distr seed = do
  tensors <- sequence $ Boxed.generate s $ \_ -> randomDoubleSeed ds size distr seed
  return $ FiniteTensor (Covariant s [d]) tensors

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
    return $ SimpleFinite (Covariant s [d]) component

randomIntSeed (d:ds) (s:size) distr seed = do
  tensors <- sequence $ Boxed.generate s $ \_ -> randomIntSeed ds size distr seed
  return $ FiniteTensor (Covariant s [d]) tensors

randomIntSeed _ _ _ _ = return $ Err invalidIndices

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
