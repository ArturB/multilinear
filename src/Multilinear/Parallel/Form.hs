{-|
Module      : Multilinear.Parallel.Form
Description : Parallel linear functional constructors (finitely- or infinitely-dimensional)
Copyright   : (c) Artur M. Brodzki, 2017
License     : GPL-3
Maintainer  : artur.brodzki@gmail.com
Stability   : experimental
Portability : Windows/POSIX

- This module provides convenient constructors that generates a parallelizable linear functionals
- Finitely-dimensional functionals provide much greater performance that infinitely-dimensional

-}

module Multilinear.Parallel.Form (
  -- * Generators
  -- ** Finite functionals
  fromIndices, Multilinear.Parallel.Form.const,
  randomDouble, randomDoubleSeed,
  randomInt, randomIntSeed,
  -- ** Infinite functionals
  fromIndices', Multilinear.Parallel.Form.const',
  -- * From files
  fromCSV, toCSV,
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
import           Multilinear.Parallel.Generic
import           Multilinear.Index.Finite   as Finite
import           Multilinear.Index.Infinite as Infinite
import           Statistics.Distribution
import qualified System.Random.MWC          as MWC

invalidIndices :: String
invalidIndices = "Indices and its sizes not compatible with structure of linear functional!"

deserializationError :: String
deserializationError = "Components deserialization error!"

-- * Finite functional generators

{-| Generate linear functional as function of indices -}
{-# INLINE fromIndices #-}
fromIndices :: (
    Num a
  ) => String        -- ^ Index name (one character)
    -> Int           -- ^ Number of elements
    -> (Int -> a)    -- ^ Generator function - returns a linear functional component at index @i@
    -> Tensor a      -- ^ Generated linear functional

fromIndices i = case i of
    [d] -> \s f -> SimpleFinite (Finite.Covariant s [d]) $ Boxed.generate s f
    _   -> \_ _ -> Err invalidIndices

{-| Generate linear functional with all components equal to some @v@ -}
{-# INLINE Multilinear.Form.const #-}
const :: (
    Num a
  ) => String      -- ^ Index name (one character)
    -> Int         -- ^ Number of elements
    -> a           -- ^ Value of each element
    -> Tensor a    -- ^ Generated linear functional

const i = case i of
    [d] -> \s v -> SimpleFinite (Finite.Covariant s [d]) $ Boxed.replicate (fromIntegral s) v
    _   -> \_ _ -> Err invalidIndices

{-| Generate linear functional with random real components with given probability distribution.
The functional is wrapped in the IO monad. -}
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
  ) => String              -- ^ Index name (one character)
    -> Int                 -- ^ Number of elements
    -> d                   -- ^ Continuous probability distribution (as from "Statistics.Distribution")
    -> IO (Tensor Double)  -- ^ Generated linear functional

randomDouble x = case x of
  [i] -> \s d -> do
    gen <- MWC.createSystemRandom
    components <- sequence $ Boxed.generate s $ \_ -> genContVar d gen
    return $ SimpleFinite (Finite.Covariant s [i]) components
  _   -> \_ _ -> return $ Err invalidIndices

{-| Generate linear functional with random integer components with given probability distribution.
The functional is wrapped in the IO monad. -}
{-| Available probability distributions: -}
{-| - Binomial : "Statistics.Distribution.Binomial" -}
{-| - Poisson : "Statistics.Distribution.Poisson" -}
{-| - Geometric : "Statistics.Distribution.Geometric" -}
{-| - Hypergeometric: "Statistics.Distribution.Hypergeometric" -}
{-# INLINE randomInt #-}
randomInt :: (
    DiscreteGen d
  ) => String             -- ^ Index name (one character)
    -> Int                -- ^ Number of elements
    -> d                  -- ^ Discrete probability distribution (as from "Statistics.Distribution")
    -> IO (Tensor Int)    -- ^ Generated linear functional

randomInt x = case x of
  [i] -> \s d -> do
    gen <- MWC.createSystemRandom
    components <- sequence $ Boxed.generate s $ \_ -> genDiscreteVar d gen
    return $ SimpleFinite (Finite.Covariant s [i]) components
  _  -> \_ _ -> return $ Err invalidIndices

{-| Generate linear functional with random real components with given probability distribution and given seed.
The functional is wrapped in a monad. -}
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
  ) => String                 -- ^ Index name (one character)
    -> Int                    -- ^ Number of elements
    -> d                      -- ^ Continuous probability distribution (as from "Statistics.Distribution")
    -> Int                    -- ^ Randomness seed
    -> m (Tensor Double)      -- ^ Generated linear functional

randomDoubleSeed x = case x of
  [i] -> \s d seed -> do
    gen <- MWC.initialize (Boxed.singleton $ fromIntegral seed)
    components <- sequence $ Boxed.generate s $ \_ -> genContVar d gen
    return $ SimpleFinite (Finite.Covariant s [i]) components
  _  -> \_ _ _ -> return $ Err invalidIndices

{-| Generate linear functional with random integer components with given probability distribution and given seed.
The functional is wrapped in a monad. -}
{-| Available probability distributions: -}
{-| - Binomial : "Statistics.Distribution.Binomial" -}
{-| - Poisson : "Statistics.Distribution.Poisson" -}
{-| - Geometric : "Statistics.Distribution.Geometric" -}
{-| - Hypergeometric: "Statistics.Distribution.Hypergeometric" -}
{-# INLINE randomIntSeed #-}
randomIntSeed :: (
    DiscreteGen d, PrimMonad m
  ) => String                -- ^ Index name (one character)
    -> Int                   -- ^ Number of elements
    -> d                     -- ^ Discrete probability distribution (as from "Statistics.Distribution")
    -> Int                   -- ^ Randomness seed
    -> m (Tensor Int)        -- ^ Generated linear functional

randomIntSeed x = case x of
  [i] -> \s d seed -> do
    gen <- MWC.initialize (Boxed.singleton $ fromIntegral seed)
    components <- sequence $ Boxed.generate s $ \_ -> genDiscreteVar d gen
    return $ SimpleFinite (Finite.Covariant s [i]) components
  _  -> \_ _ _ -> return $ Err invalidIndices




-- ** Infinite functionals generators




{-| Generate linear functional as function of indices -}
{-# INLINE fromIndices' #-}
fromIndices' :: (
    Num a
  ) => String        -- ^ Index name (one character)
    -> (Int -> a)    -- ^ Generator function - returns a linear functional component at index @i@
    -> Tensor a      -- ^ Generated linear functional

fromIndices' i = case i of
    [d] -> \f -> InfiniteTensor (Infinite.Covariant [d]) $ (Scalar . f) <$> [0..]
    _   -> \_ -> Err invalidIndices

{-| Generate linear functional with all components equal to some @v@ -}
{-# INLINE Multilinear.Form.const' #-}
const' :: (
    Num a
  ) => String      -- ^ Index name (one character)
    -> a           -- ^ Value of each element
    -> Tensor a    -- ^ Generated linear functional

const' i = case i of
    [d] -> \v -> InfiniteTensor (Infinite.Covariant [d]) $ (\_ -> Scalar v) <$> ([0..] :: [Int])
    _   -> \_ -> Err invalidIndices



-- * From files



{-| Read linear functional components from CSV file. Reads only the first row of the file. -}
{-# INLINE fromCSV #-}
fromCSV :: (
    Num a, NFData a, Serialize a
  ) => String                                    -- ^ Index name (one character)
    -> String                                    -- ^ CSV file name
    -> Char                                      -- ^ Separator expected to be used in this CSV file
    -> EitherT SomeException IO (Tensor a)       -- ^ Generated linear functional or error message

fromCSV x = case x of
  [i] -> \fileName separator -> do
    csv <- EitherT $ readCSVFile (CSVS separator (Just '"') (Just '"') separator) fileName
    let firstLine = head csv
    let components = decode <$> firstLine
    let readSize = length $ rights components
    if readSize > 0
    then return $ SimpleFinite (Finite.Covariant readSize [i]) $ Boxed.fromList (rights components)
    else EitherT $ return $ Left $ SomeException $ TypeError deserializationError
  _  -> \_ _ -> return $ Err invalidIndices

{-| Write linear functional to CSV file. -}
{-# INLINE toCSV #-}
toCSV :: (
    Num a, NFData a, Serialize a
  ) => Tensor a   -- ^ Functional to serialize
    -> String     -- ^ CSV file name
    -> Char       -- ^ Separator expected to be used in this CSV file
    -> IO Int     -- ^ Number of rows written

toCSV t = case t of
  (FiniteTensor (Finite.Covariant _ _) elems) -> \fileName separator ->
    let encodedElems = [Boxed.toList $ encode <$> elems]
    in
      if length (indices t) == 1
      then writeCSVFile (CSVS separator (Just '"') (Just '"') separator) fileName encodedElems
      else return 0
  _ -> \_ _ -> return 0

