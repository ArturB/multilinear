{-|
Module      : Multilinear.Vector
Description : Vector constructors (finitely- or infinitely-dimensional)
Copyright   : (c) Artur M. Brodzki, 2017
License     : GPL-3
Maintainer  : artur.brodzki@gmail.com
Stability   : experimental
Portability : Windows/POSIX

- This module provides convenient constructors that generates a vector (tensor with one upper index) in array ("Data.Vector") implementation
- Finitely-dimensional vectors provide much greater performance than infinitely-dimensional 

-}

module Multilinear.Vector (
  -- * Generators
  fromIndices, Multilinear.Vector.const,
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
import           Multilinear.Index.Finite
import           Statistics.Distribution
import qualified System.Random.MWC          as MWC

{-| Generate linear functional as function of indices -}
fromIndices :: (
    Eq a, Show a, Num a, Bits a
  ) => String             -- ^ Index name (one character)
    -> Int                -- ^ Number of elements
    -> (Int -> a)         -- ^ Generator function - returns a linear functional component at index @i@
    -> Tensor a      -- ^ Generated linear functional

fromIndices [d] s f = mergeScalars $ 
    FiniteTensor (Contravariant s [d]) $ Boxed.generate s (Scalar . f)
fromIndices _ _ _ = Err "Indices and its sizes not compatible with structure of linear functional!"

{-| Generate linear functional with all components equal to some @v@ -}
const :: (
    Eq a, Show a, Num a, Bits a
  ) => String           -- ^ Index name (one character)
    -> Int              -- ^ Number of elements
    -> a                -- ^ Value of each element
    -> Tensor a     -- ^ Generated linear functional

const [d] s v = mergeScalars $ 
    FiniteTensor (Contravariant s [d]) $ Boxed.replicate (fromIntegral s) (Scalar v)
const _ _ _ = Err "Indices and its sizes not compatible with structure of linear functional!"

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
randomDouble :: (
    ContGen d
  ) => String                  -- ^ Index name (one character)
    -> Int                     -- ^ Number of elements
    -> d                       -- ^ Continuous probability distribution (as from "Statistics.Distribution")
    -> IO (Tensor Double)  -- ^ Generated linear functional

randomDouble [i] s d = do
  components <- sequence [ MWC.withSystemRandom . MWC.asGenIO $ \gen -> genContVar d gen | _ <- [1..s] ]
  return $  mergeScalars $ FiniteTensor (Contravariant s [i]) $ Scalar <$> Boxed.fromList components
randomDouble _ _ _ = return $ Err "Indices and its sizes not compatible with structure of linear functional!"

{-| Generate linear functional with random integer components with given probability distribution.
The functional is wrapped in the IO monad. -}
{-| Available probability distributions: -}
{-| - Binomial : "Statistics.Distribution.Binomial" -}
{-| - Poisson : "Statistics.Distribution.Poisson" -}
{-| - Geometric : "Statistics.Distribution.Geometric" -}
{-| - Hypergeometric: "Statistics.Distribution.Hypergeometric" -}
randomInt :: (
    DiscreteGen d
  ) => String                 -- ^ Index name (one character)
    -> Int                    -- ^ Number of elements
    -> d                      -- ^ Discrete probability distribution (as from "Statistics.Distribution")
    -> IO (Tensor Int)    -- ^ Generated linear functional

randomInt [i] s d = do
  components <- sequence [ MWC.withSystemRandom . MWC.asGenIO $ \gen -> genDiscreteVar d gen | _ <- [1..s] ]
  return $  mergeScalars $ FiniteTensor (Contravariant s [i]) $ Scalar <$> Boxed.fromList components
randomInt _ _ _ = return $ Err "Indices and its sizes not compatible with structure of linear functional!"

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
randomDoubleSeed :: (
    ContGen d, PrimMonad m
  ) => String                     -- ^ Index name (one character)
    -> Int                        -- ^ Number of elements
    -> d                          -- ^ Continuous probability distribution (as from "Statistics.Distribution")
    -> Int                        -- ^ Randomness seed
    -> m (Tensor Double)      -- ^ Generated linear functional

randomDoubleSeed [i] s d seed = do
  gen <- MWC.initialize (Boxed.singleton $ fromIntegral seed)
  components <- sequence [ genContVar d gen | _ <- [1..s] ]
  return $  mergeScalars $ FiniteTensor (Contravariant s [i]) $ Scalar <$> Boxed.fromList components
randomDoubleSeed _ _ _ _ = return $ Err "Indices and its sizes not compatible with structure of linear functional!"

{-| Generate linear functional with random integer components with given probability distribution and given seed.
The functional is wrapped in a monad. -}
{-| Available probability distributions: -}
{-| - Binomial : "Statistics.Distribution.Binomial" -}
{-| - Poisson : "Statistics.Distribution.Poisson" -}
{-| - Geometric : "Statistics.Distribution.Geometric" -}
{-| - Hypergeometric: "Statistics.Distribution.Hypergeometric" -}
randomIntSeed :: (
    DiscreteGen d, PrimMonad m
  ) => String                    -- ^ Index name (one character)
    -> Int                       -- ^ Number of elements
    -> d                         -- ^ Discrete probability distribution (as from "Statistics.Distribution")
    -> Int                       -- ^ Randomness seed
    -> m (Tensor Int)        -- ^ Generated linear functional

randomIntSeed [i] s d seed = do
  gen <- MWC.initialize (Boxed.singleton $ fromIntegral seed)
  components <- sequence [ genDiscreteVar d gen | _ <- [1..s] ]
  return $  mergeScalars $ FiniteTensor (Contravariant s [i]) $ Scalar <$> Boxed.fromList components
randomIntSeed _ _ _ _ = return $ Err "Indices and its sizes not compatible with structure of linear functional!"

{-| Read linear functional components from CSV file. Reads only the first row of the file. -}
fromCSV :: (
    Eq a, Show a, Num a, Bits a, Serialize a
  ) => String                                         -- ^ Index name (one character)
    -> String                                         -- ^ CSV file name
    -> Char                                           -- ^ Separator expected to be used in this CSV file
    -> EitherT SomeException IO (Tensor a)       -- ^ Generated linear functional or error message

fromCSV [i] fileName separator = do
  csv <- EitherT $ readCSVFile (CSVS separator (Just '"') (Just '"') separator) fileName
  let firstLine = head csv
  let components = decode <$> firstLine
  let readSize = length $ rights components
  if readSize > 0
  then return $  mergeScalars $ FiniteTensor (Contravariant readSize [i]) (Scalar <$> Boxed.fromList (rights components))
  else EitherT $ return $ Left $ SomeException $ TypeError "Components deserialization error!"
fromCSV _ _ _ = return $ Err "Indices and its sizes not compatible with structure of linear functional!"

{-| Write linear functional to CSV file. -}
toCSV :: (
    Eq a, Show a, Num a, Bits a, Serialize a
  ) => Tensor a    -- ^ Functional to serialize
    -> String          -- ^ CSV file name
    -> Char            -- ^ Separator expected to be used in this CSV file
    -> IO Int          -- ^ Number of rows written

toCSV t@(FiniteTensor (Contravariant _ _)  elems) fileName separator =
  let encodedElems = [Boxed.toList $ encode <$> elems]
  in
    if length (indices t) == 1
    then writeCSVFile (CSVS separator (Just '"') (Just '"') separator) fileName encodedElems
    else return 0
toCSV _ _ _ = return 0

