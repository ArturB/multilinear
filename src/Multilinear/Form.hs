{-|
Module      : Form
Description : Linear functional
Copyright   : (c) Artur M. Brodzki, 2017
License     : GPL-3
Maintainer  : artur.brodzki@gmail.com
Stability   : experimental
Portability : Windows/POSIX

This module provides convenient constructors that generates a linear functionals (tensor with one lower index). 

-}

{-# LANGUAGE Strict #-}

module Multilinear.Form (
  fromIndices, Multilinear.Form.const,
  randomDouble, randomDoubleSeed,
  randomInt, randomIntSeed,
  fromCSV, toCSV  
) where

import           Multilinear.Generic.AsList
import           Multilinear
import           Data.Bits
import           Data.Either
import           Data.Serialize
import           Data.CSV.Enumerator
import           Control.Monad.Primitive
import           Control.Monad.Trans.Either
import           Control.Exception
import           Statistics.Distribution
import qualified System.Random.MWC         as MWC
import qualified Data.Vector               as Vector

{-| Generate linear functional as function of indices -}
fromIndices :: (
    Eq i, Show i, Integral i,
    Eq a, Show a, Num a, Bits a
  ) => String      -- ^ Index name (one character)
    -> i           -- ^ Number of elements
    -> (i -> a)    -- ^ Generator function - returns a linear functional component at index @i@
    -> Tensor i a  -- ^ Generated linear functional

fromIndices [d] s f =
    Tensor (Covariant s [d]) [Scalar $ f x | x <- [0 .. s - 1] ]
fromIndices _ _ _ = Err "Indices and its sizes not compatible with structure of 1-form!"

{-| Generate linear functional with all components equal to some @v@ -}
const :: (
    Eq i, Show i, Integral i,
    Eq a, Show a, Num a, Bits a
  ) => String       -- ^ Index name (one character)
    -> i            -- ^ Number of elements
    -> a            -- ^ Value of each element
    -> Tensor i a   -- ^ Generated linear functional

const [d] s v = 
    Tensor (Covariant s [d]) $ replicate (fromIntegral s) (Scalar v)
const _ _ _ = Err "Indices and its sizes not compatible with structure of 1-form!"

{-| Generate linear functional with random real components with given probability distribution. The form is wrapped in the IO monad. -}
randomDouble :: (
    Eq i, Show i, Integral i,
    ContGen d
  ) => String                -- ^ Index name (one character)
    -> i                     -- ^ Number of elements
    -> d                     -- ^ Continuous probability distribution (as from "Statistics.Distribution")
    -> IO (Tensor i Double)  -- ^ Generated linear functional

randomDouble [i] s d = do
  components <- sequence [ MWC.withSystemRandom . MWC.asGenIO $ \gen -> genContVar d gen | _ <- [1..s] ]
  return $ Tensor (Covariant s [i]) $ Scalar <$> components
randomDouble _ _ _ = return $ Err "Indices and its sizes not compatible with structure of 1-form!"

{-| Generate linear functional with random integer components with given probability distribution. The form is wrapped in the IO monad. -}
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

{-| Generate linear functional with random real components with given probability distribution and given seed. The form is wrapped in a monad. -}
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

{-| Generate linear functional with random integer components with given probability distribution and given seed. The form is wrapped in a monad. -}
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

{-| Read linear functional components from CSV file. Reads only the first row of the file. -}
fromCSV :: (
    Eq a, Show a, Num a, Bits a, Serialize a
  ) => String                                  -- ^ Index name (one character)
    -> String                                  -- ^ CSV file name
    -> Char                                    -- ^ Separator expected to be used in this CSV file
    -> EitherT SomeException IO (Tensor Int a) -- ^ Generated linear functional or error message

fromCSV [i] fileName separator = do
  csv <- EitherT $ readCSVFile (CSVS separator (Just '"') (Just '"') separator) fileName
  let firstLine = head csv
  let components = decode <$> firstLine
  let size = length $ rights components
  if size > 0
  then return $ Tensor (Covariant size [i]) (Scalar <$> rights components)
  else EitherT $ return $ Left $ SomeException $ TypeError "Components deserialization error!"
fromCSV _ _ _ = return $ Err "Indices and its sizes not compatible with structure of 1-form!"

{-| Write linear functional to CSV file. -}
toCSV :: (
    Eq i, Show i, Integral i, Serialize i, 
    Eq a, Show a, Num a, Bits a, Serialize a
  ) => Tensor i a  -- ^ Functional to serialize
    -> String      -- ^ CSV file name
    -> Char        -- ^ Separator expected to be used in this CSV file
    -> IO Int      -- ^ Number of rows written

toCSV t@(Tensor (Covariant _ _) elems) fileName separator = 
  let encodedElems = [encode <$> elems]
  in  
    if length (indices t) == 1
    then writeCSVFile (CSVS separator (Just '"') (Just '"') separator) fileName encodedElems
    else return 0
toCSV _ _ _ = return 0