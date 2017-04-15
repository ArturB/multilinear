{-|
Module      : Vector
Description : Vector
Copyright   : (c) Artur M. Brodzki, 2017
License     : GPL-3
Maintainer  : artur.brodzki@gmail.com
Stability   : experimental
Portability : Windows/POSIX

This module provides convenient constructors that generates a vector (tensor with one upper index).

-}

{-# LANGUAGE GADTs  #-}
{-# LANGUAGE Strict #-}

module Multilinear.Vector (
  fromIndices, Multilinear.Vector.const,
  randomDouble, randomDoubleSeed,
  randomInt, randomIntSeed,
  fromCSV, toCSV
) where

import           Control.Exception
import           Control.Monad.Primitive
import           Control.Monad.Trans.Either
import           Data.Bits
import           Data.CSV.Enumerator
import           Data.Either
import           Data.Serialize
import qualified Data.Vector                as Vector
import           Multilinear
import           Multilinear.Generic.AsList
import           Statistics.Distribution
import qualified System.Random.MWC          as MWC

{-| Generate vector as function of indices -}
fromIndices :: (
    Eq i, Show i, Integral i,
    Eq a, Show a, Num a, Bits a
  ) => String      -- ^ Index name (one character)
    -> i           -- ^ Number of elements
    -> (i -> a)    -- ^ Generator function - returns a vector component at index @i@
    -> Tensor i a  -- ^ Generated vector

fromIndices [d] s f =
    Tensor (Contravariant s [d]) [Scalar $ f x | x <- [0 .. s - 1] ]
fromIndices _ _ _ = Err "Indices and its sizes not compatible with structure of vector!"

{-| Generate vector with all components equal to some @v@ -}
const :: (
    Eq i, Show i, Integral i,
    Eq a, Show a, Num a, Bits a
  ) => String       -- ^ Index name (one character)
    -> i            -- ^ Number of elements
    -> a            -- ^ Value of each element
    -> Tensor i a   -- ^ Generated vector

const [d] s v =
    Tensor (Contravariant s [d]) $ replicate (fromIntegral s) (Scalar v)
const _ _ _ = Err "Indices and its sizes not compatible with structure of vector!"

{-| Generate vector with random real components with given probability distribution.
The vector is wrapped in the IO monad. -}
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
randomDouble :: (
    Eq i, Show i, Integral i,
    ContGen d
  ) => String                -- ^ Index name (one character)
    -> i                     -- ^ Number of elements
    -> d                     -- ^ Continuous probability distribution (as from "Statistics.Distribution")
    -> IO (Tensor i Double)  -- ^ Generated vector

randomDouble [i] s d = do
  components <- sequence [ MWC.withSystemRandom . MWC.asGenIO $ \gen -> genContVar d gen | _ <- [1..s] ]
  return $ Tensor (Contravariant s [i]) $ Scalar <$> components
randomDouble _ _ _ = return $ Err "Indices and its sizes not compatible with structure of vector!"

{-| Generate vector with random integer components with given probability distribution.
The vector is wrapped in the IO monad. -}
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
    -> IO (Tensor i Int)  -- ^ Generated vector

randomInt [i] s d = do
  components <- sequence [ MWC.withSystemRandom . MWC.asGenIO $ \gen -> genDiscreteVar d gen | _ <- [1..s] ]
  return $ Tensor (Contravariant s [i]) $ Scalar <$> components
randomInt _ _ _ = return $ Err "Indices and its sizes not compatible with structure of vector!"

{-| Generate vector with random real components with given probability distribution and given seed.
The vector is wrapped in a monad. -}
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
randomDoubleSeed :: (
    Eq i, Show i, Integral i,
    ContGen d, Integral i2, PrimMonad m
  ) => String              -- ^ Index name (one character)
    -> i                   -- ^ Number of elements
    -> d                   -- ^ Continuous probability distribution (as from "Statistics.Distribution")
    -> i2                  -- ^ Randomness seed
    -> m (Tensor i Double) -- ^ Generated vector

randomDoubleSeed [i] s d seed = do
  gen <- MWC.initialize (Vector.singleton $ fromIntegral seed)
  components <- sequence [ genContVar d gen | _ <- [1..s] ]
  return $ Tensor (Contravariant s [i]) $ Scalar <$> components
randomDoubleSeed _ _ _ _ = return $ Err "Indices and its sizes not compatible with structure of vector!"

{-| Generate vector with random integer components with given probability distribution and given seed.
The vector is wrapped in a monad. -}
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
    -> m (Tensor i Int)   -- ^ Generated vector

randomIntSeed [i] s d seed = do
  gen <- MWC.initialize (Vector.singleton $ fromIntegral seed)
  components <- sequence [ genDiscreteVar d gen | _ <- [1..s] ]
  return $ Tensor (Contravariant s [i]) $ Scalar <$> components
randomIntSeed _ _ _ _ = return $ Err "Indices and its sizes not compatible with structure of vector!"

{-| Read vector components from CSV file. Reads only the first row of the file. -}
fromCSV :: (
    Eq a, Show a, Num a, Bits a, Serialize a
  ) => String                                  -- ^ Index name (one character)
    -> String                                  -- ^ CSV file name
    -> Char                                    -- ^ Separator expected to be used in this CSV file
    -> EitherT SomeException IO (Tensor Int a) -- ^ Generated vector or error message

fromCSV [i] fileName separator = do
  csv <- EitherT $ readCSVFile (CSVS separator (Just '"') (Just '"') separator) fileName
  let firstLine = head csv
  let components = decode <$> firstLine
  let size = length components
  if size > 0
  then return $ Tensor (Contravariant size [i]) (Scalar <$> rights components)
  else EitherT $ return $ Left $ SomeException $ TypeError "Components deserialization error!"
fromCSV _ _ _ = return $ Err "Indices and its sizes not compatible with structure of vector!"

{-| Write vector to CSV file. -}
toCSV :: (
    Eq i, Show i, Integral i, Serialize i,
    Eq a, Show a, Num a, Bits a, Serialize a
  ) => Tensor i a  -- ^ Vector to serialize
    -> String      -- ^ CSV file name
    -> Char        -- ^ Separator expected to be used in this CSV file
    -> IO Int      -- ^ Number of rows written

toCSV t@(Tensor (Contravariant _ _) elems) fileName separator =
  let encodedElems = [encode <$> elems]
  in
    if length (indices t) == 1
    then writeCSVFile (CSVS separator (Just '"') (Just '"') separator) fileName encodedElems
    else return 0
toCSV _ _ _ = return 0
