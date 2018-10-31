{-|
Module      : Multilinear.Matrix
Description : Matrix constructors (finitely- or infinitely dimensional)
Copyright   : (c) Artur M. Brodzki, 2017
License     : GPL-3
Maintainer  : artur.brodzki@gmail.com
Stability   : experimental
Portability : Windows/POSIX

- This module provides convenient constructors that generates a matrix (finitely- or infinite-dimensional)
- Finitely-dimensional matrices provide much greater performance than infinitely-dimensional

-}

module Multilinear.Matrix (
  -- * Generators
  Multilinear.Matrix.fromIndices, 
  Multilinear.Matrix.const,
  Multilinear.Matrix.randomDouble, 
  Multilinear.Matrix.randomDoubleSeed,
  Multilinear.Matrix.randomInt, 
  Multilinear.Matrix.randomIntSeed,
  -- * From files
  Multilinear.Matrix.fromCSV, 
  Multilinear.Matrix.toCSV
) where

import           Control.DeepSeq
import           Control.Exception
import           Control.Monad.Primitive
import           Control.Monad.Trans.Either
import           Data.CSV.Enumerator
import           Data.Either
import           Data.Serialize
import qualified Data.Vector                as Boxed
import           Multilinear.Class          as Multilinear
import           Multilinear.Generic
import           Multilinear.Index.Finite   as Finite
import qualified Multilinear.Tensor         as Tensor
import           Statistics.Distribution

invalidIndices :: String
invalidIndices = "Indices and its sizes not compatible with structure of matrix!"

deserializationError :: String
deserializationError = "Components deserialization error!"

{-| Generate matrix as function of its indices -}
{-# INLINE fromIndices #-}
fromIndices :: (
    Num a
  ) => String               -- ^ Indices names (one character per index, first character: rows index, second character: columns index)
    -> Int                  -- ^ Number of matrix rows
    -> Int                  -- ^ Number of matrix columns
    -> (Int -> Int -> a)    -- ^ Generator function - returns a matrix component at @i,j@
    -> Tensor a             -- ^ Generated matrix

fromIndices [u,d] us ds f = Tensor.fromIndices ([u],[us]) ([d],[ds]) $ \[ui] [di] -> f ui di
fromIndices _ _ _ _ = Err invalidIndices

{-| Generate matrix with all components equal to @v@ -}
{-# INLINE Multilinear.Matrix.const #-}
const :: (
    Num a
  ) => String    -- ^ Indices names (one character per index, first character: rows index, second character: columns index)
    -> Int       -- ^ Number of matrix rows
    -> Int       -- ^ Number of matrix columns
    -> a         -- ^ Value of matrix components
    -> Tensor a  -- ^ Generated matrix

const [u,d] us ds = Tensor.const ([u],[us]) ([d],[ds])
const _ _ _ = \_ -> Err invalidIndices

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
{-# INLINE randomDouble #-}
randomDouble :: (
    ContGen d
  ) => String              -- ^ Indices names (one character per index, first character: rows index, second character: columns index)
    -> Int                 -- ^ Number of matrix rows
    -> Int                 -- ^ Number of matrix columns
    -> d                   -- ^ Continuous probability distribution (as from "Statistics.Distribution")
    -> IO (Tensor Double)  -- ^ Generated matrix

randomDouble [u,d] us ds = Tensor.randomDouble ([u],[us]) ([d],[ds])
randomDouble _ _ _ = \_ -> return $ Err invalidIndices

{-| Generate matrix with random integer components with given probability distribution.
The matrix is wrapped in the IO monad. -}
{-| Available probability distributions: -}
{-| - Binomial : "Statistics.Distribution.Binomial" -}
{-| - Poisson : "Statistics.Distribution.Poisson" -}
{-| - Geometric : "Statistics.Distribution.Geometric" -}
{-| - Hypergeometric: "Statistics.Distribution.Hypergeometric" -}
{-# INLINE randomInt #-}
randomInt :: (
    DiscreteGen d
  ) => String           -- ^ Indices names (one character per index, first character: rows index, second character: columns index)
    -> Int              -- ^ Number of matrix rows
    -> Int              -- ^ Number of matrix columns
    -> d                -- ^ Discrete probability distribution (as from "Statistics.Distribution")
    -> IO (Tensor Int)  -- ^ Generated matrix

randomInt [u,d] us ds = Tensor.randomInt ([u],[us]) ([d],[ds])
randomInt _ _ _ = \_ -> return $ Err invalidIndices

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
{-# INLINE randomDoubleSeed #-}
randomDoubleSeed :: (
    ContGen d, PrimMonad m
  ) => String              -- ^ Indices names (one character per index, first character: rows index, second character: columns index)
    -> Int                 -- ^ Number of matrix rows
    -> Int                 -- ^ Number of matrix columns
    -> d                   -- ^ Continuous probability distribution (as from "Statistics.Distribution")
    -> Int                 -- ^ Randomness seed
    -> m (Tensor Double)   -- ^ Generated matrix

randomDoubleSeed [u,d] us ds = Tensor.randomDoubleSeed ([u],[us]) ([d],[ds])
randomDoubleSeed _ _ _ = \_ _ -> return $ Err invalidIndices

{-| Generate matrix with random integer components with given probability distribution. and given seed.
The matrix is wrapped in a monad. -}
{-| Available probability distributions: -}
{-| - Binomial : "Statistics.Distribution.Binomial" -}
{-| - Poisson : "Statistics.Distribution.Poisson" -}
{-| - Geometric : "Statistics.Distribution.Geometric" -}
{-| - Hypergeometric: "Statistics.Distribution.Hypergeometric" -}
{-# INLINE randomIntSeed #-}
randomIntSeed :: (
    DiscreteGen d, PrimMonad m
  ) => String              -- ^ Indices names (one character per index, first character: rows index, second character: columns index)
    -> Int                 -- ^ Number of matrix rows
    -> Int                 -- ^ Number of matrix columns
    -> d                   -- ^ Discrete probability distribution (as from "Statistics.Distribution")
    -> Int                 -- ^ Randomness seed
    -> m (Tensor Int)      -- ^ Generated matrix

randomIntSeed [u,d] us ds = Tensor.randomIntSeed ([u],[us]) ([d],[ds])
randomIntSeed _ _ _ = \_ _ -> return $ Err invalidIndices

{-| Read matrix components from CSV file. -}
{-# INLINE fromCSV #-}
fromCSV :: (
    Num a, NFData a, Serialize a
  ) => String                                  -- ^ Indices names (one character per index, first character: rows index, second character: columns index)
    -> String                                  -- ^ CSV file name
    -> Char                                    -- ^ Separator expected to be used in this CSV file
    -> EitherT SomeException IO (Tensor a)     -- ^ Generated matrix or error message

fromCSV x = case x of
  [u,d] -> \fileName separator -> do
    csv <- EitherT $ readCSVFile (CSVS separator (Just '"') (Just '"') separator) fileName
    let components = (decode <$> ) <$> csv
    let rows = length components
    let columns = if rows > 0 then length $ rights (head components) else 0
    if rows > 0 && columns > 0
    then return $ 
      FiniteTensor (Finite.Contravariant rows [u]) $ (
        SimpleFinite (Finite.Covariant columns [d]) . Boxed.fromList . rights
      ) <$> Boxed.fromList components
    else EitherT $ return $ Left $ SomeException $ TypeError deserializationError

  _ -> \_ _ -> return $ Err invalidIndices

{-| Write matrix to CSV file. -}
{-# INLINE toCSV                    #-}
toCSV :: (
    Num a, NFData a, Serialize a
  ) => Tensor a  -- ^ Matrix to serialize
    -> String    -- ^ CSV file name
    -> Char      -- ^ Separator expected to be used in this CSV file
    -> IO Int    -- ^ Number of rows written

toCSV t = case order t of
  (1,1) -> \fileName separator ->
    let t' = _standardize t
        elems = Boxed.toList $ Boxed.toList . tensorScalars <$> tensorsFinite t'
        encodedElems = (encode <$>) <$> elems
    in  writeCSVFile (CSVS separator (Just '"') (Just '"') separator) fileName encodedElems

  _ -> \_ _ -> return 0
