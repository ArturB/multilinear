{-|
Module      : Multilinear.NForm
Description : N-Forms, dot and cross product and determinant
Copyright   : (c) Artur M. Brodzki, 2017
License     : GLP-3
Maintainer  : artur.brodzki@gmail.com
Stability   : experimental
Portability : Windows/POSIX

This module provides convenient constructors that generates n-forms (tensors with n lower indices).

-}

{-# LANGUAGE GADTs  #-}
{-# LANGUAGE Strict #-}
{-# OPTIONS_GHC #-}

module Multilinear.NForm (
  fromIndices, Multilinear.NForm.const,
  {-randomDouble,-} randomDoubleSeed,
  randomInt, randomIntSeed,
  dot, cross
) where

import           Control.Monad.Primitive
import           Data.Bits
import qualified Data.Vector                as Vector
import           Multilinear
import           Multilinear.Generic.AsList
import qualified Multilinear.Tensor         as Tensor
import           Statistics.Distribution
import qualified System.Random.MWC          as MWC

{-| Generate N-form as function of its indices -}
fromIndices :: (
    Eq i, Show i, Integral i,
    Eq a, Show a, Num a, Bits a
  ) => String      -- ^ Indices names (one characted per index)
    -> [i]         -- ^ Indices sizes
    -> ([i] -> a)  -- ^ Generator function
    -> Tensor i a  -- ^ Generated N-form

fromIndices [] [] f = Scalar $ f []
fromIndices (d:ds) (s:size) f =
    Tensor (Covariant s [d]) [fromIndices ds size (\dss -> f (x:dss)) | x <- [0 .. s - 1] ]
fromIndices _ _ _ = Err "Indices and its sizes incompatible with n-form structure!"

{-| Generate N-form with all components equal to @v@ -}
const :: (
    Eq i, Show i, Integral i,
    Eq a, Show a, Num a, Bits a
  ) => String      -- ^ Indices names (one characted per index)
    -> [i]         -- ^ Indices sizes
    -> a           -- ^ N-form elements value
    -> Tensor i a  -- ^ Generated N-form

const [] [] v = Scalar v
const (d:ds) (s:size) v =
    Tensor (Covariant s [d]) $ replicate (fromIntegral s) $ Multilinear.NForm.const ds size v
const _ _ _ = Err "Indices and its sizes incompatible with n-form structure!"

{-| 2-form representing a dot product -}
dot :: (
    Eq i, Show i, Integral i,
    Eq a, Show a, Num a, Bits a
  ) => String      -- ^ Indices names (one characted per index)
    -> i           -- ^ Size of tensor (dot product is a square tensor)
    -> Tensor i a  -- ^ Generated dot product

dot [i1,i2] size = fromIndices [i1,i2] [size,size] (\[i,j] -> if i == j then 1 else 0)
dot _ _ = Err "Indices and its sizes incompatible with dot product!"

{-| Tensor representing a cross product (Levi - Civita symbol). It also allows to compute a determinant of square matrix - determinant of matrix @M@ is a equal to length of cross product of all columns of @M@ -}
-- // TODO
cross :: (
    Eq i, Show i, Integral i,
    Eq a, Show a, Num a, Bits a
  ) => String      -- ^ Indices names (one characted per index)
    -> i           -- ^ Size of tensor (dot product is a square tensor)
    -> Tensor i a  -- ^ Generated dot product

cross [i,j,k] size =
  Tensor.fromIndices ([i],[size]) ([j,k],[size,size])
    (\[_] [_,_] -> 0)
cross _ _ = Err "Indices and its sizes incompatible with cross product!"

{-| Generate linear functional with random real components with given probability distribution.
The form is wrapped in the IO monad. -}
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
  ) => String                -- ^ Indices names (one character per index)
    -> [i]                   -- ^ Indices sizes
    -> d                     -- ^ Continuous probability distribution (as from "Statistics.Distribution")
    -> IO (Tensor i Double)  -- ^ Generated linear functional

randomDouble [] [] d = do
    component <- MWC.withSystemRandom . MWC.asGenIO $ \gen -> genContVar d gen
    return $ Scalar component

randomDouble [i] s d = do
  components <- sequence [ MWC.withSystemRandom . MWC.asGenIO $ \gen -> genContVar d gen | _ <- [1..s] ]
  return $ Tensor (Covariant s [i]) $ Scalar <$> components

randomDouble _ _ _ = return $ Err "Indices and its sizes not compatible with structure of 1-form!"

{-| Generate linear functional with random integer components with given probability distribution.
The form is wrapped in the IO monad. -}
{-| Available probability distributions: -}
{-| - Binomial : "Statistics.Distribution.Binomial" -}
{-| - Poisson : "Statistics.Distribution.Poisson" -}
{-| - Geometric : "Statistics.Distribution.Geometric" -}
{-| - Hypergeometric: "Statistics.Distribution.Hypergeometric" -}
randomInt :: (
    Eq i, Show i, Integral i,
    DiscreteGen d
  ) => String             -- ^ Index name (one character)
    -> i                  -- ^ Number of elements
    -> d                  -- ^ Discrete probability distribution (as from "Statistics.Distribution")
    -> IO (Tensor i Int)  -- ^ Generated linear functional

randomInt [i] s d = do
  components <- sequence [ MWC.withSystemRandom . MWC.asGenIO $ \gen -> genDiscreteVar d gen | _ <- [1..s] ]
  return $ Tensor (Covariant s [i]) $ Scalar <$> components
randomInt _ _ _ = return $ Err "Indices and its sizes not compatible with structure of 1-form!"

{-| Generate linear functional with random real components with given probability distribution and given seed.
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
    Eq i, Show i, Integral i,
    ContGen d, Integral i2, PrimMonad m
  ) => String              -- ^ Index name (one character)
    -> i                   -- ^ Number of elements
    -> d                   -- ^ Continuous probability distribution (as from "Statistics.Distribution")
    -> i2                  -- ^ Randomness seed
    -> m (Tensor i Double) -- ^ Generated linear functional

randomDoubleSeed [i] s d seed = do
  gen <- MWC.initialize (Vector.singleton $ fromIntegral seed)
  components <- sequence [ genContVar d gen | _ <- [1..s] ]
  return $ Tensor (Covariant s [i]) $ Scalar <$> components
randomDoubleSeed _ _ _ _ = return $ Err "Indices and its sizes not compatible with structure of 1-form!"

{-| Generate linear functional with random integer components with given probability distribution and given seed.
The form is wrapped in a monad. -}
{-| Available probability distributions: -}
{-| - Binomial : "Statistics.Distribution.Binomial" -}
{-| - Poisson : "Statistics.Distribution.Poisson" -}
{-| - Geometric : "Statistics.Distribution.Geometric" -}
{-| - Hypergeometric: "Statistics.Distribution.Hypergeometric" -}
randomIntSeed :: (
    Eq i, Show i, Integral i,
    DiscreteGen d, Integral i2, PrimMonad m
  ) => String             -- ^ Index name (one character)
    -> i                  -- ^ Number of elements
    -> d                  -- ^ Discrete probability distribution (as from "Statistics.Distribution")
    -> i2                 -- ^ Randomness seed
    -> m (Tensor i Int)   -- ^ Generated linear functional

randomIntSeed [i] s d seed = do
  gen <- MWC.initialize (Vector.singleton $ fromIntegral seed)
  components <- sequence [ genDiscreteVar d gen | _ <- [1..s] ]
  return $ Tensor (Covariant s [i]) $ Scalar <$> components
randomIntSeed _ _ _ _ = return $ Err "Indices and its sizes not compatible with structure of 1-form!"

