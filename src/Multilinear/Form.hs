{-|
Module      : Form
Description : Implements a Form datatype - a tensor with only covariant indices, especially 1-covector: linear functional.
Copyright   : (c) Artur M. Brodzki, 2017
License     : 3-clause BSD
Maintainer  : artur.brodzki@gmail.com
Stability   : experimental
Portability : Windows/POSIX

-}

{-# LANGUAGE GADTs  #-}
{-# LANGUAGE Strict #-}

module Multilinear.Form (
  fromIndices, 
  randomDouble, randomDoubleSeed,
  randomInt, randomIntSeed,
  Multilinear.Form.const 
  --elf
) where

import           Multilinear.Generic.AsList
import           Multilinear.Index
import           Data.Bits
import           Control.Monad.Primitive
import           Statistics.Distribution
import qualified System.Random.MWC         as MWC
import qualified Data.Vector               as Vector

{-| Generate form as function of indices |-}
fromIndices :: (
    Eq i, Show i, Integral i,
    Eq a, Show a, Num a, Bits a
  ) => String -> i -> (i -> a) -> Tensor i a

fromIndices [d] s f =
    Tensor (Covariant s [d]) [Scalar $ f x | x <- [0 .. s - 1] ]
fromIndices _ _ _ = Err "Indices and its sizes not compatible with structure of 1-form!"

{-| Generate form with all s components equal to v |-}
const :: (
    Eq i, Show i, Integral i,
    Eq a, Show a, Num a, Bits a
  ) => String -> i -> a -> Tensor i a

const [d] s v = 
    Tensor (Covariant s [d]) $ replicate (fromIntegral s) (Scalar v)
const _ _ _ = Err "Indices and its sizes not compatible with structure of 1-form!"

{- Generate form with random real components with given probability distribution. The form is wrapped in IO monad. |-}
randomDouble :: (
    Eq i, Show i, Integral i,
    ContGen d
  ) => String -> i -> d -> IO (Tensor i Double)

randomDouble [i] s d = do
  components <- sequence [ MWC.withSystemRandom . MWC.asGenIO $ \gen -> genContVar d gen | _ <- [1..s] ]
  return $ Tensor (Covariant s [i]) $ Scalar <$> components
randomDouble _ _ _ = return $ Err "Indices and its sizes not compatible with structure of 1-form!"

{- Generate form with random integer components with given probability distribution. The form is wrapped in IO monad. |-}
randomInt :: (
    Eq i, Show i, Integral i,
    DiscreteGen d
  ) => String -> i -> d -> IO (Tensor i Int)

randomInt [i] s d = do
  components <- sequence [ MWC.withSystemRandom . MWC.asGenIO $ \gen -> genDiscreteVar d gen | _ <- [1..s] ]
  return $ Tensor (Covariant s [i]) $ Scalar <$> components
randomInt _ _ _ = return $ Err "Indices and its sizes not compatible with structure of 1-form!"

{- Generate form with random real components with given probability distribution and given seed. The form is wrapped in a monad. |-}
randomDoubleSeed :: (
    Eq i, Show i, Integral i,
    ContGen d, Integral i2, PrimMonad m
  ) => String -> i -> d -> i2 -> m (Tensor i Double)

randomDoubleSeed [i] s d seed = do
  gen <- MWC.initialize (Vector.singleton $ fromIntegral seed)
  components <- sequence [ genContVar d gen | _ <- [1..s] ]
  return $ Tensor (Covariant s [i]) $ Scalar <$> components
randomDoubleSeed _ _ _ _ = return $ Err "Indices and its sizes not compatible with structure of 1-form!"

{- Generate form with random integer components with given probability distribution and given seed. The form is wrapped in a monad. |-}
randomIntSeed :: (
    Eq i, Show i, Integral i,
    DiscreteGen d, Integral i2, PrimMonad m
  ) => String -> i -> d -> i2 -> m (Tensor i Int)

randomIntSeed [i] s d seed = do
  gen <- MWC.initialize (Vector.singleton $ fromIntegral seed)
  components <- sequence [ genDiscreteVar d gen | _ <- [1..s] ]
  return $ Tensor (Covariant s [i]) $ Scalar <$> components
randomIntSeed _ _ _ _ = return $ Err "Indices and its sizes not compatible with structure of 1-form!"

{-| Read form components from CSV file. Reads only the first row of the file. |-}

