{-|
Module      : Multilinear.Matrix
Description : Matrix
Copyright   : (c) Artur M. Brodzki, 2017
License     : GPL-3
Maintainer  : artur.brodzki@gmail.com
Stability   : experimental
Portability : Windows/POSIX

This module provides convenient constructors that generates a matrix.

-}

{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict              #-}
{-# OPTIONS_GHC #-}

module Multilinear.Matrix (
  fromIndices, Multilinear.Matrix.const,
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

{-| Generate matrix as function of its indices -}
fromIndices :: (
    Eq i, Show i, Integral i,
    Eq a, Show a, Integral a, Num a, Bits a
  ) => String         -- ^ Indices names (one character per index, first character: rows index, second character: columns index)
    -> i              -- ^ Number of matrix rows
    -> i              -- ^ Number of matrix columns
    -> (i -> i -> a)  -- ^ Generator function - returns a matrix component at @i,j@
    -> Tensor i a     -- ^ Generated matrix

fromIndices [u,d] su sd f =
    Tensor (Contravariant su [u])
      [Tensor (Covariant sd [d])
        [Scalar $ f x y
      | y <- [0 .. sd - 1] ]
    | x <- [0 .. su - 1] ]
fromIndices _ _ _ _ = Err "Indices and its sizes incompatible with matrix structure!"

{-| Generate matrix with all components equal to @v@ -}
const :: (
    Eq i, Show i, Integral i,
    Eq a, Show a, Num a, Bits a
  ) => String      -- ^ Indices names (one character per index, first character: rows index, second character: columns index)
    -> i           -- ^ Number of matrix rows
    -> i           -- ^ Number of matrix columns
    -> a           -- ^ Value of matrix components
    -> Tensor i a  -- ^ Generated matrix

const [u,d] su sd v =
    Tensor (Contravariant su [u]) $
      replicate (fromIntegral su) $
        Tensor (Covariant sd [d]) $
          replicate (fromIntegral sd) $ Scalar v
const _ _ _ _ = Err "Indices and its sizes incompatible with matrix structure!"

{-| Generate matrix with random real components with given probability distribution.
The matrix is wrapped in the IO monad. -}
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
  ) => String                -- ^ Indices names (one character per index, first character: rows index, second character: columns index)
    -> i                     -- ^ Number of matrix rows
    -> i                     -- ^ Number of matrix columns
    -> d                     -- ^ Continuous probability distribution (as from "Statistics.Distribution")
    -> IO (Tensor i Double)  -- ^ Generated matrix

randomDouble [u,d] su sd dist = do
  components <-
    sequence [
      sequence [
        MWC.withSystemRandom . MWC.asGenIO $ \gen -> genContVar dist gen
      | _ <- [1..sd] ]
    | _ <- [1..su] ]

  return $
    Tensor (Contravariant su [u]) $ (\x ->
      Tensor (Covariant sd [d]) $ Scalar <$> x
    ) <$> components
randomDouble _ _ _ _ = return $ Err "Indices and its sizes not compatible with structure of matrix!"

{-| Generate matrix with random integer components with given probability distribution.
The matrix is wrapped in the IO monad. -}
{-| Available probability distributions: -}
{-| - Binomial : "Statistics.Distribution.Binomial" -}
{-| - Poisson : "Statistics.Distribution.Poisson" -}
{-| - Geometric : "Statistics.Distribution.Geometric" -}
{-| - Hypergeometric: "Statistics.Distribution.Hypergeometric" -}
randomInt :: (
    Eq i, Show i, Integral i,
    DiscreteGen d
  ) => String                -- ^ Indices names (one character per index, first character: rows index, second character: columns index)
    -> i                     -- ^ Number of matrix rows
    -> i                     -- ^ Number of matrix columns
    -> d                     -- ^ Discrete probability distribution (as from "Statistics.Distribution")
    -> IO (Tensor i Double)  -- ^ Generated matrix

randomInt [u,d] su sd dist = do
  components <-
    sequence [
      sequence [
        MWC.withSystemRandom . MWC.asGenIO $ \gen -> genContVar dist gen
      | _ <- [1..sd] ]
    | _ <- [1..su] ]

  return $
    Tensor (Contravariant su [u]) $ (\x ->
      Tensor (Covariant sd [d]) $ Scalar <$> x
    ) <$> components
randomInt _ _ _ _ = return $ Err "Indices and its sizes not compatible with structure of matrix!"

{-| Generate matrix with random real components with given probability distribution and given seed.
The matrix is wrapped in the a monad. -}
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
  ) => String                -- ^ Indices names (one character per index, first character: rows index, second character: columns index)
    -> i                     -- ^ Number of matrix rows
    -> i                     -- ^ Number of matrix columns
    -> d                     -- ^ Continuous probability distribution (as from "Statistics.Distribution")
    -> i2                    -- ^ Randomness seed
    -> m (Tensor i Double)   -- ^ Generated matrix

randomDoubleSeed [u,d] su sd dist seed = do
  gen <- MWC.initialize (Vector.singleton $ fromIntegral seed)
  components <-
    sequence [
      sequence [
        genContVar dist gen
      | _ <- [1..sd] ]
    | _ <- [1..su] ]

  return $
    Tensor (Contravariant su [u]) $ (\x ->
      Tensor (Covariant sd [d]) $ Scalar <$> x
    ) <$> components
randomDoubleSeed _ _ _ _ _ = return $ Err "Indices and its sizes not compatible with structure of 1-form!"

{-| Generate matrix with random integer components with given probability distribution. and given seed.
The matrix is wrapped in a monad. -}
{-| Available probability distributions: -}
{-| - Binomial : "Statistics.Distribution.Binomial" -}
{-| - Poisson : "Statistics.Distribution.Poisson" -}
{-| - Geometric : "Statistics.Distribution.Geometric" -}
{-| - Hypergeometric: "Statistics.Distribution.Hypergeometric" -}
randomIntSeed :: (
    Eq i, Show i, Integral i,
    DiscreteGen d, Integral i2, PrimMonad m
  ) => String                -- ^ Indices names (one character per index, first character: rows index, second character: columns index)
    -> i                     -- ^ Number of matrix rows
    -> i                     -- ^ Number of matrix columns
    -> d                     -- ^ Discrete probability distribution (as from "Statistics.Distribution")
    -> i2                    -- ^ Randomness seed
    -> m (Tensor i Int)      -- ^ Generated matrix

randomIntSeed [u,d] su sd dist seed = do
  gen <- MWC.initialize (Vector.singleton $ fromIntegral seed)
  components <-
    sequence [
      sequence [
        genDiscreteVar dist gen
      | _ <- [1..sd] ]
    | _ <- [1..su] ]

  return $
    Tensor (Contravariant su [u]) $ (\x ->
      Tensor (Covariant sd [d]) $ Scalar <$> x
    ) <$> components
randomIntSeed _ _ _ _ _ = return $ Err "Indices and its sizes not compatible with structure of matrix!"

{-| Read matrix components from CSV file. -}
fromCSV :: (
    Eq a, Show a, Num a, Bits a, Serialize a
  ) => String                                  -- ^ Indices names (one character per index, first character: rows index, second character: columns index)
    -> String                                  -- ^ CSV file name
    -> Char                                    -- ^ Separator expected to be used in this CSV file
    -> EitherT SomeException IO (Tensor Int a) -- ^ Generated matrix or error message

fromCSV [u,d] fileName separator = do
  csv <- EitherT $ readCSVFile (CSVS separator (Just '"') (Just '"') separator) fileName
  let components = (decode <$> ) <$> csv
  let rows = length components
  let columns = if rows > 0 then length $ rights (head components) else 0
  if rows > 0 && columns > 0
  then return $
    Tensor (Contravariant rows [u]) $ (\x ->
      Tensor (Covariant columns [d]) $ Scalar <$> rights x
    ) <$> components
  else EitherT $ return $ Left $ SomeException $ TypeError "Components deserialization error!"
fromCSV _ _ _ = return $ Err "Indices and its sizes not compatible with structure of 1-form!"

{-| Write matrix to CSV file. -}
toCSV :: (
    Eq i, Show i, Integral i, Serialize i,
    Eq a, Show a, Num a, Bits a, Serialize a
  ) => Tensor i a  -- ^ Matrix to serialize
    -> String      -- ^ CSV file name
    -> Char        -- ^ Separator expected to be used in this CSV file
    -> IO Int      -- ^ Number of rows written

toCSV t@(Tensor (Contravariant _ _) rows) fileName separator =
  let elems = tensorData <$> rows
      encodedElems = (encode . scalarVal <$>) <$> elems
  in
    if length (indices t) == 2 && isCovariant (indices t !! 1)
    then writeCSVFile (CSVS separator (Just '"') (Just '"') separator) fileName encodedElems
    else return 0
toCSV _ _ _ = return 0
