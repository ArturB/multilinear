{-|
Module      : Multilinear.Matrix
Description : Matrix constructors (finitely- or infinitely dimensional)
Copyright   : (c) Artur M. Brodzki, 2017
License     : GPL-3
Maintainer  : artur.brodzki@gmail.com
Stability   : experimental
Portability : Windows/POSIX

- This module provides convenient constructors that generates a matrix
- Finitely-dimensional matrices provide much greater performance than infinitely-dimensional

-}

module Multilinear.Matrix (
  -- * Generators
fromIndices, Multilinear.Matrix.const,
  randomDouble, randomDoubleSeed,
  randomInt, randomIntSeed,
  -- * From files
  fromCSV, toCSV
) where

import           Control.Exception
import           Control.Monad.Primitive
import           Control.Monad.Trans.Either
import           Data.Bits
import           Data.CSV.Enumerator
import           Data.Either
import           Data.Serialize
import qualified Data.Vector                as Boxed
import           Multilinear
import           Multilinear.Generic
import qualified Multilinear.Index          as TIndex
import           Multilinear.Index.Finite
import           Statistics.Distribution
import qualified System.Random.MWC          as MWC

{-| Generate matrix as function of its indices -}
fromIndices :: (
    Eq a, Show a, Integral a, Num a, Bits a
  ) => String               -- ^ Indices names (one character per index, first character: rows index, second character: columns index)
    -> Int                  -- ^ Number of matrix rows
    -> Int                  -- ^ Number of matrix columns
    -> (Int -> Int -> a)    -- ^ Generator function - returns a matrix component at @i,j@
    -> Tensor a       -- ^ Generated matrix

fromIndices [u,d] su sd f = mergeScalars $
    FiniteTensor (Contravariant su [u]) $
      Boxed.generate su (\i -> FiniteTensor (Covariant sd [d]) $
        Boxed.generate sd (Scalar . f i)
     )
fromIndices _ _ _ _ = Err "Indices and its sizes incompatible with matrix structure!"

{-| Generate matrix with all components equal to @v@ -}
const :: (
    Eq a, Show a, Num a, Bits a
  ) => String          -- ^ Indices names (one character per index, first character: rows index, second character: columns index)
    -> Int             -- ^ Number of matrix rows
    -> Int             -- ^ Number of matrix columns
    -> a               -- ^ Value of matrix components
    -> Tensor a  -- ^ Generated matrix

const [u,d] su sd v = mergeScalars $
    FiniteTensor (Contravariant su [u]) $
      Boxed.replicate (fromIntegral su) $
        FiniteTensor (Covariant sd [d]) $
          Boxed.replicate (fromIntegral sd) $ Scalar v
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
    ContGen d
  ) => String                    -- ^ Indices names (one character per index, first character: rows index, second character: columns index)
    -> Int                       -- ^ Number of matrix rows
    -> Int                       -- ^ Number of matrix columns
    -> d                         -- ^ Continuous probability distribution (as from "Statistics.Distribution")
    -> IO (Tensor Double)  -- ^ Generated matrix

randomDouble [u,d] su sd dist = do
  components <-
    sequence [
      sequence [
        MWC.withSystemRandom . MWC.asGenIO $ \gen -> genContVar dist gen
      | _ <- [1..sd] ]
    | _ <- [1..su] ]

  return $ mergeScalars $
    FiniteTensor (Contravariant su [u]) $ (\x ->
      FiniteTensor (Covariant sd [d]) $ Scalar <$> Boxed.fromList x
    ) <$> Boxed.fromList components
randomDouble _ _ _ _ = return $ Err "Indices and its sizes not compatible with structure of matrix!"

{-| Generate matrix with random integer components with given probability distribution.
The matrix is wrapped in the IO monad. -}
{-| Available probability distributions: -}
{-| - Binomial : "Statistics.Distribution.Binomial" -}
{-| - Poisson : "Statistics.Distribution.Poisson" -}
{-| - Geometric : "Statistics.Distribution.Geometric" -}
{-| - Hypergeometric: "Statistics.Distribution.Hypergeometric" -}
randomInt :: (
    DiscreteGen d
  ) => String                    -- ^ Indices names (one character per index, first character: rows index, second character: columns index)
    -> Int                       -- ^ Number of matrix rows
    -> Int                       -- ^ Number of matrix columns
    -> d                         -- ^ Discrete probability distribution (as from "Statistics.Distribution")
    -> IO (Tensor Double)  -- ^ Generated matrix

randomInt [u,d] su sd dist = do
  components <-
    sequence [
      sequence [
        MWC.withSystemRandom . MWC.asGenIO $ \gen -> genContVar dist gen
      | _ <- [1..sd] ]
    | _ <- [1..su] ]

  return $ mergeScalars $
    FiniteTensor (Contravariant su [u]) $ (\x ->
      FiniteTensor (Covariant sd [d]) $ Scalar <$> Boxed.fromList x
    ) <$> Boxed.fromList components
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
    ContGen d, PrimMonad m
  ) => String                    -- ^ Indices names (one character per index, first character: rows index, second character: columns index)
    -> Int                       -- ^ Number of matrix rows
    -> Int                       -- ^ Number of matrix columns
    -> d                         -- ^ Continuous probability distribution (as from "Statistics.Distribution")
    -> Int                       -- ^ Randomness seed
    -> m (Tensor Double)   -- ^ Generated matrix

randomDoubleSeed [u,d] su sd dist seed = do
  gen <- MWC.initialize (Boxed.singleton $ fromIntegral seed)
  components <-
    sequence [
      sequence [
        genContVar dist gen
      | _ <- [1..sd] ]
    | _ <- [1..su] ]

  return $ mergeScalars $
    FiniteTensor (Contravariant su [u]) $ (\x ->
      FiniteTensor (Covariant sd [d]) $ Scalar <$> Boxed.fromList x
    ) <$> Boxed.fromList components
randomDoubleSeed _ _ _ _ _ = return $ Err "Indices and its sizes not compatible with structure of matrix!"

{-| Generate matrix with random integer components with given probability distribution. and given seed.
The matrix is wrapped in a monad. -}
{-| Available probability distributions: -}
{-| - Binomial : "Statistics.Distribution.Binomial" -}
{-| - Poisson : "Statistics.Distribution.Poisson" -}
{-| - Geometric : "Statistics.Distribution.Geometric" -}
{-| - Hypergeometric: "Statistics.Distribution.Hypergeometric" -}
randomIntSeed :: (
    DiscreteGen d, PrimMonad m
  ) => String                    -- ^ Indices names (one character per index, first character: rows index, second character: columns index)
    -> Int                       -- ^ Number of matrix rows
    -> Int                       -- ^ Number of matrix columns
    -> d                         -- ^ Discrete probability distribution (as from "Statistics.Distribution")
    -> Int                       -- ^ Randomness seed
    -> m (Tensor Int)      -- ^ Generated matrix

randomIntSeed [u,d] su sd dist seed = do
  gen <- MWC.initialize (Boxed.singleton $ fromIntegral seed)
  components <-
    sequence [
      sequence [
        genDiscreteVar dist gen
      | _ <- [1..sd] ]
    | _ <- [1..su] ]

  return $ mergeScalars $
    FiniteTensor (Contravariant su [u]) $ (\x ->
      FiniteTensor (Covariant sd [d]) $ Scalar <$> Boxed.fromList x
    ) <$> Boxed.fromList components
randomIntSeed _ _ _ _ _ = return $ Err "Indices and its sizes not compatible with structure of matrix!"

{-| Read matrix components from CSV file. -}
fromCSV :: (
    Eq a, Show a, Num a, Bits a, Serialize a
  ) => String                                        -- ^ Indices names (one character per index, first character: rows index, second character: columns index)
    -> String                                        -- ^ CSV file name
    -> Char                                          -- ^ Separator expected to be used in this CSV file
    -> EitherT SomeException IO (Tensor a)     -- ^ Generated matrix or error message

fromCSV [u,d] fileName separator = do
  csv <- EitherT $ readCSVFile (CSVS separator (Just '"') (Just '"') separator) fileName
  let components = (decode <$> ) <$> csv
  let rows = length components
  let columns = if rows > 0 then length $ rights (head components) else 0
  if rows > 0 && columns > 0
  then return $ mergeScalars $
    FiniteTensor (Contravariant rows [u]) $ (\x ->
      FiniteTensor (Covariant columns [d]) $ Scalar <$> Boxed.fromList (rights x)
    ) <$> Boxed.fromList components
  else EitherT $ return $ Left $ SomeException $ TypeError "Components deserialization error!"
fromCSV _ _ _ = return $ Err "Indices and its sizes not compatible with structure of matrix!"

{-| Write matrix to CSV file. -}
toCSV :: (
    Eq a, Show a, Num a, Bits a, Serialize a
  ) => Tensor a  -- ^ Matrix to serialize
    -> String          -- ^ CSV file name
    -> Char            -- ^ Separator expected to be used in this CSV file
    -> IO Int          -- ^ Number of rows written

toCSV t@(FiniteTensor (Contravariant _ _) rows) fileName separator =
  let elems = Boxed.toList $ Boxed.toList . tensorsFinite <$> rows
      encodedElems = (encode . scalarVal <$>) <$> elems
  in
    if length (indices t) == 2 && TIndex.isCovariant (indices t !! 1)
    then writeCSVFile (CSVS separator (Just '"') (Just '"') separator) fileName encodedElems
    else return 0
toCSV _ _ _ = return 0
