{-|
Module      : Multilinear.Matrix.AsList
Description : Matrix in list implementation
Copyright   : (c) Artur M. Brodzki, 2017
License     : GPL-3
Maintainer  : artur.brodzki@gmail.com
Stability   : experimental
Portability : Windows/POSIX

- This module provides convenient constructors that generates a matrix in list implementation. 
- Choice of container type has great impact on library performance in particular use cases
- Array ("Data.Vector") implementation is generally faster, however it is strict and always keeps all tensor elements in memory, so it may require large amount of RAM.
- List implementation is slower but lazy and when tensor is generated from indices or randomly, it does not generate all elements at once if not necessary,
so it may operate in smaller memory (e.g. linear instead of quadratic when multiplying matrix by vector or form).

-}

module Multilinear.Matrix.AsList (
  -- * Finite matrices
  fromIndices, Multilinear.Matrix.AsList.const,
  randomDouble, randomDoubleSeed,
  randomInt, randomIntSeed,
  -- * Infinite matrices
  fromIndices', Multilinear.Matrix.AsList.const',
  randomDouble', randomDoubleSeed',
  randomInt', randomIntSeed',
  -- * From files
  fromCSV, toCSV
) where

import           Control.Applicative
import           Control.Exception
import           Control.Monad.Primitive
import           Control.Monad.Trans.Either
import           Data.Bits
import           Data.CSV.Enumerator
import           Data.Either
import           Data.Serialize
import qualified Data.Vector                as Vector
import           Multilinear
import           Multilinear.Generic
import           Multilinear.Generic.AsList
import           Multilinear.Index
import           Statistics.Distribution
import qualified System.Random.MWC          as MWC

-- * FINITE MATRICES

{-| Generate finite matrix as function of its indices -}
fromIndices :: (
    Eq a, Show a, Integral a, Num a, Bits a
  ) => String             -- ^ Indices names (one character per index, first character: rows index, second character: columns index)
    -> Int                -- ^ Number of matrix rows
    -> Int                -- ^ Number of matrix columns
    -> (Int -> Int -> a)  -- ^ Generator function - returns a matrix component at @i,j@
    -> ListTensor a       -- ^ Generated matrix

fromIndices [u,d] su sd f =
    FiniteTensor (Contravariant (Just su) [u]) $
      ZipList [FiniteTensor (Covariant (Just sd) [d]) $
        ZipList [Scalar $ f x y
      | y <- [0 .. sd - 1] ]
    | x <- [0 .. su - 1] ]
fromIndices _ _ _ _ = Err "Indices and its sizes incompatible with matrix structure!"

{-| Generate finite matrix with all components equal to @v@ -}
const :: (
    Eq a, Show a, Num a, Bits a
  ) => String        -- ^ Indices names (one character per index, first character: rows index, second character: columns index)
    -> Int             -- ^ Number of matrix rows
    -> Int             -- ^ Number of matrix columns
    -> a             -- ^ Value of matrix components
    -> ListTensor a  -- ^ Generated matrix

const [u,d] su sd v =
    FiniteTensor (Contravariant (Just su) [u]) $
      ZipList $ replicate (fromIntegral su) $
        FiniteTensor (Covariant (Just sd) [d]) $
          ZipList $ replicate (fromIntegral sd) $ Scalar v
const _ _ _ _ = Err "Indices and its sizes incompatible with matrix structure!"

{-| Generate finite matrix with random real components with given probability distribution.
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
  ) => String                  -- ^ Indices names (one character per index, first character: rows index, second character: columns index)
    -> Int                       -- ^ Number of matrix rows
    -> Int                       -- ^ Number of matrix columns
    -> d                       -- ^ Continuous probability distribution (as from "Statistics.Distribution")
    -> IO (ListTensor Double)  -- ^ Generated matrix

randomDouble [u,d] su sd dist = do
  components <-
    sequence [
      sequence [
        MWC.withSystemRandom . MWC.asGenIO $ \gen -> genContVar dist gen
      | _ <- [1..sd] ]
    | _ <- [1..su] ]

  return $
    FiniteTensor (Contravariant (Just su) [u]) $ ZipList $ (\x ->
      FiniteTensor (Covariant (Just sd) [d]) $ ZipList $ Scalar <$> x
    ) <$> components
randomDouble _ _ _ _ = return $ Err "Indices and its sizes not compatible with structure of matrix!"

{-| Generate finite matrix with random integer components with given probability distribution.
The matrix is wrapped in the IO monad. -}
{-| Available probability distributions: -}
{-| - Binomial : "Statistics.Distribution.Binomial" -}
{-| - Poisson : "Statistics.Distribution.Poisson" -}
{-| - Geometric : "Statistics.Distribution.Geometric" -}
{-| - Hypergeometric: "Statistics.Distribution.Hypergeometric" -}
randomInt :: (
    DiscreteGen d
  ) => String                  -- ^ Indices names (one character per index, first character: rows index, second character: columns index)
    -> Int                       -- ^ Number of matrix rows
    -> Int                       -- ^ Number of matrix columns
    -> d                       -- ^ Discrete probability distribution (as from "Statistics.Distribution")
    -> IO (ListTensor Double)  -- ^ Generated matrix

randomInt [u,d] su sd dist = do
  components <-
    sequence [
      sequence [
        MWC.withSystemRandom . MWC.asGenIO $ \gen -> genContVar dist gen
      | _ <- [1..sd] ]
    | _ <- [1..su] ]

  return $
    FiniteTensor (Contravariant (Just su) [u]) $ ZipList $ (\x ->
      FiniteTensor (Covariant (Just sd) [d]) $ ZipList $ Scalar <$> x
    ) <$> components
randomInt _ _ _ _ = return $ Err "Indices and its sizes not compatible with structure of matrix!"

{-| Generate finite matrix with random real components with given probability distribution and given seed.
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
  ) => String                  -- ^ Indices names (one character per index, first character: rows index, second character: columns index)
    -> Int                       -- ^ Number of matrix rows
    -> Int                       -- ^ Number of matrix columns
    -> d                       -- ^ Continuous probability distribution (as from "Statistics.Distribution")
    -> Int                      -- ^ Randomness seed
    -> m (ListTensor Double)   -- ^ Generated matrix

randomDoubleSeed [u,d] su sd dist seed = do
  gen <- MWC.initialize (Vector.singleton $ fromIntegral seed)
  components <-
    sequence [
      sequence [
        genContVar dist gen
      | _ <- [1..sd] ]
    | _ <- [1..su] ]

  return $
    FiniteTensor (Contravariant (Just su) [u]) $ ZipList $ (\x ->
      FiniteTensor (Covariant (Just sd) [d]) $ ZipList $ Scalar <$> x
    ) <$> components
randomDoubleSeed _ _ _ _ _ = return $ Err "Indices and its sizes not compatible with structure of matrix!"

{-| Generate finite matrix with random integer components with given probability distribution. and given seed.
The matrix is wrapped in a monad. -}
{-| Available probability distributions: -}
{-| - Binomial : "Statistics.Distribution.Binomial" -}
{-| - Poisson : "Statistics.Distribution.Poisson" -}
{-| - Geometric : "Statistics.Distribution.Geometric" -}
{-| - Hypergeometric: "Statistics.Distribution.Hypergeometric" -}
randomIntSeed :: (
    DiscreteGen d, PrimMonad m
  ) => String                  -- ^ Indices names (one character per index, first character: rows index, second character: columns index)
    -> Int                       -- ^ Number of matrix rows
    -> Int                       -- ^ Number of matrix columns
    -> d                       -- ^ Discrete probability distribution (as from "Statistics.Distribution")
    -> Int                      -- ^ Randomness seed
    -> m (ListTensor Int)      -- ^ Generated matrix

randomIntSeed [u,d] su sd dist seed = do
  gen <- MWC.initialize (Vector.singleton $ fromIntegral seed)
  components <-
    sequence [
      sequence [
        genDiscreteVar dist gen
      | _ <- [1..sd] ]
    | _ <- [1..su] ]

  return $
    FiniteTensor (Contravariant (Just su) [u]) $ ZipList $ (\x ->
      FiniteTensor (Covariant (Just sd) [d]) $ ZipList $ Scalar <$> x
    ) <$> components
randomIntSeed _ _ _ _ _ = return $ Err "Indices and its sizes not compatible with structure of matrix!"

-- * INFINITE MATRICES

{-| Generate infinite matrix as function of its indices -}
fromIndices' :: (
    Eq a, Show a, Integral a, Num a, Bits a
  ) => String             -- ^ Indices names (one character per index, first character: rows index, second character: columns index)
    -> (Int -> Int -> a)  -- ^ Generator function - returns a matrix component at @i,j@
    -> ListTensor a       -- ^ Generated matrix

fromIndices' [u,d] f =
    FiniteTensor (Contravariant Nothing [u]) $
      ZipList [FiniteTensor (Covariant Nothing [d]) $
        ZipList [Scalar $ f x y
      | y <- [0 .. ] ]
    | x <- [0 .. ] ]
fromIndices' _ _ = Err "Indices and its sizes incompatible with matrix structure!"

{-| Generate infinite matrix with all components equal to @v@ -}
const' :: (
    Eq a, Show a, Num a, Bits a
  ) => String        -- ^ Indices names (one character per index, first character: rows index, second character: columns index)
    -> a             -- ^ Value of matrix components
    -> ListTensor a  -- ^ Generated matrix

const' [u,d] v =
    FiniteTensor (Contravariant Nothing [u]) $
      ZipList [FiniteTensor (Covariant Nothing [d]) $
          ZipList [Scalar v | _ <- [0 .. ] ]
      | _ <- [0 .. ] ]
const' _ _ = Err "Indices and its sizes incompatible with matrix structure!"

{-| Generate infinite matrix with random real components with given probability distribution.
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
randomDouble' :: (
    ContGen d
  ) => String                  -- ^ Indices names (one character per index, first character: rows index, second character: columns index)
    -> d                       -- ^ Continuous probability distribution (as from "Statistics.Distribution")
    -> IO (ListTensor Double)  -- ^ Generated matrix

randomDouble' [u,d] dist = do
  components <-
    sequence [
      sequence [
        MWC.withSystemRandom . MWC.asGenIO $ \gen -> genContVar dist gen
      | _ <- [1..] ]
    | _ <- [1..] ]

  return $
    FiniteTensor (Contravariant Nothing [u]) $ ZipList $ (\x ->
      FiniteTensor (Covariant Nothing [d]) $ ZipList $ Scalar <$> x
    ) <$> components
randomDouble' _ _ = return $ Err "Indices and its sizes not compatible with structure of matrix!"

{-| Generate infinite matrix with random integer components with given probability distribution.
The matrix is wrapped in the IO monad. -}
{-| Available probability distributions: -}
{-| - Binomial : "Statistics.Distribution.Binomial" -}
{-| - Poisson : "Statistics.Distribution.Poisson" -}
{-| - Geometric : "Statistics.Distribution.Geometric" -}
{-| - Hypergeometric: "Statistics.Distribution.Hypergeometric" -}
randomInt' :: (
    DiscreteGen d
  ) => String                  -- ^ Indices names (one character per index, first character: rows index, second character: columns index)
    -> d                       -- ^ Discrete probability distribution (as from "Statistics.Distribution")
    -> IO (ListTensor Double)  -- ^ Generated matrix

randomInt' [u,d] dist = do
  components <-
    sequence [
      sequence [
        MWC.withSystemRandom . MWC.asGenIO $ \gen -> genContVar dist gen
      | _ <- [1..] ]
    | _ <- [1..] ]

  return $
    FiniteTensor (Contravariant Nothing [u]) $ ZipList $ (\x ->
      FiniteTensor (Covariant Nothing [d]) $ ZipList $ Scalar <$> x
    ) <$> components
randomInt' _ _ = return $ Err "Indices and its sizes not compatible with structure of matrix!"

{-| Generate infinite matrix with random real components with given probability distribution and given seed.
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
randomDoubleSeed' :: (
    ContGen d, PrimMonad m
  ) => String                  -- ^ Indices names (one character per index, first character: rows index, second character: columns index)
    -> d                       -- ^ Continuous probability distribution (as from "Statistics.Distribution")
    -> Int                     -- ^ Randomness seed
    -> m (ListTensor Double)   -- ^ Generated matrix

randomDoubleSeed' [u,d] dist seed = do
  gen <- MWC.initialize (Vector.singleton $ fromIntegral seed)
  components <-
    sequence [
      sequence [
        genContVar dist gen
      | _ <- [1..] ]
    | _ <- [1..] ]

  return $
    FiniteTensor (Contravariant Nothing [u]) $ ZipList $ (\x ->
      FiniteTensor (Covariant Nothing [d]) $ ZipList $ Scalar <$> x
    ) <$> components
randomDoubleSeed' _ _ _ = return $ Err "Indices and its sizes not compatible with structure of matrix!"

{-| Generate infinite matrix with random integer components with given probability distribution. and given seed.
The matrix is wrapped in a monad. -}
{-| Available probability distributions: -}
{-| - Binomial : "Statistics.Distribution.Binomial" -}
{-| - Poisson : "Statistics.Distribution.Poisson" -}
{-| - Geometric : "Statistics.Distribution.Geometric" -}
{-| - Hypergeometric: "Statistics.Distribution.Hypergeometric" -}
randomIntSeed' :: (
    DiscreteGen d, PrimMonad m
  ) => String                  -- ^ Indices names (one character per index, first character: rows index, second character: columns index)
    -> d                       -- ^ Discrete probability distribution (as from "Statistics.Distribution")
    -> Int                      -- ^ Randomness seed
    -> m (ListTensor Int)      -- ^ Generated matrix

randomIntSeed' [u,d] dist seed = do
  gen <- MWC.initialize (Vector.singleton $ fromIntegral seed)
  components <-
    sequence [
      sequence [
        genDiscreteVar dist gen
      | _ <- [1..] ]
    | _ <- [1..] ]

  return $
    FiniteTensor (Contravariant Nothing [u]) $ ZipList $ (\x ->
      FiniteTensor (Covariant Nothing [d]) $ ZipList $ Scalar <$> x
    ) <$> components
randomIntSeed' _ _ _ = return $ Err "Indices and its sizes not compatible with structure of matrix!"

{-| Read matrix components from CSV file. -}
fromCSV :: (
    Eq a, Show a, Num a, Bits a, Serialize a
  ) => String                                      -- ^ Indices names (one character per index, first character: rows index, second character: columns index)
    -> String                                      -- ^ CSV file name
    -> Char                                        -- ^ Separator expected to be used in this CSV file
    -> EitherT SomeException IO (ListTensor a)     -- ^ Generated matrix or error message

fromCSV [u,d] fileName separator = do
  csv <- EitherT $ readCSVFile (CSVS separator (Just '"') (Just '"') separator) fileName
  let components = (decode <$> ) <$> csv
  let rows = length components
  let columns = if rows > 0 then length $ rights (head components) else 0
  if rows > 0 && columns > 0
  then return $
    FiniteTensor (Contravariant (Just rows) [u]) $ ZipList $ (\x ->
      FiniteTensor (Covariant (Just columns) [d]) $ ZipList $ Scalar <$> rights x
    ) <$> components
  else EitherT $ return $ Left $ SomeException $ TypeError "Components deserialization error!"
fromCSV _ _ _ = return $ Err "Indices and its sizes not compatible with structure of matrix!"

{-| Write matrix to CSV file. -}
toCSV :: (
    Eq a, Show a, Num a, Bits a, Serialize a
  ) => ListTensor a  -- ^ Matrix to serialize
    -> String        -- ^ CSV file name
    -> Char          -- ^ Separator expected to be used in this CSV file
    -> IO Int        -- ^ Number of rows written

toCSV t@(FiniteTensor (Contravariant _ _) (ZipList rows)) fileName separator =
  let elems = getZipList . tensorData <$> rows
      encodedElems = (encode . scalarVal <$>) <$> elems
  in
    if length (indices t) == 2 && isCovariant (indices t !! 1)
    then writeCSVFile (CSVS separator (Just '"') (Just '"') separator) fileName encodedElems
    else return 0
toCSV _ _ _ = return 0
