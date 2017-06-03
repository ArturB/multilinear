{-|
Module      : Multilinear.NForm.AsArray
Description : N-Forms, dot and cross product and determinant in array ("Data.Vector") implementation
Copyright   : (c) Artur M. Brodzki, 2017
License     : GLP-3
Maintainer  : artur.brodzki@gmail.com
Stability   : experimental
Portability : Windows/POSIX

- This module provides convenient constructors that generates n-forms (tensors with n lower indices) in array ("Data.Vector") implementation.
- Choice of container type has great impact on library performance in particular use cases
- Array ("Data.Vector") implementation is generally faster, however it is strict and always keeps all tensor elements in memory, so it may require large amount of RAM.
- List implementation is slower but lazy and when tensor is generated from indices or randomly, it does not generate all elements at once if not necessary,
so it may operate in smaller memory (e.g. linear instead of quadratic when multiplying matrix by vector or form).


-}

--{-# LANGUAGE Strict #-}

module Multilinear.NForm.AsArray (
    -- * Generators
  fromIndices, Multilinear.NForm.AsArray.const,
  randomDouble, randomDoubleSeed,
  randomInt, randomIntSeed,
  -- * Common cases
  dot, cross
) where


import           Control.Monad.Primitive
import           Data.Bits
import qualified Data.Vector                 as Boxed
import           Multilinear.Generic
import           Multilinear.Generic.AsArray
import           Multilinear.Index.Finite
import qualified Multilinear.Tensor.AsArray  as Tensor
import           Statistics.Distribution
import qualified System.Random.MWC           as MWC

{-| Generate N-form as function of its indices -}
fromIndices :: (
    Eq a, Show a, Num a, Bits a
  ) => String          -- ^ Indices names (one characted per index)
    -> [Int]           -- ^ Indices sizes
    -> ([Int] -> a)    -- ^ Generator function
    -> VectorTensor a  -- ^ Generated N-form

fromIndices [] [] f = Scalar $ f []
fromIndices (d:ds) (s:size) f = mergeScalars $ 
    FiniteTensor (Covariant s [d]) $ ZipVector $ Boxed.generate s (\x -> fromIndices ds size (\dss -> f (x:dss)) )
fromIndices _ _ _ = Err "Indices and its sizes incompatible with n-vector structure!"

{-| Generate N-form with all components equal to @v@ -}
const :: (
    Eq a, Show a, Num a, Bits a
  ) => String          -- ^ Indices names (one characted per index)
    -> [Int]           -- ^ Indices sizes
    -> a               -- ^ N-form elements value
    -> VectorTensor a  -- ^ Generated N-form

const [] [] v = Scalar v
const (d:ds) (s:size) v = mergeScalars $ 
    FiniteTensor (Covariant s [d]) $ ZipVector $ Boxed.replicate (fromIntegral s) $ Multilinear.NForm.AsArray.const ds size v
const _ _ _ = Err "Indices and its sizes incompatible with n-vector structure!"

{-| Generate n-vector with random real components with given probability distribution.
The n-vector is wrapped in the IO monad. -}
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
    ContGen d
  ) => String                    -- ^ Indices names (one character per index)
    -> [Int]                     -- ^ Indices sizes
    -> d                         -- ^ Continuous probability distribution (as from "Statistics.Distribution")
    -> IO (VectorTensor Double)  -- ^ Generated linear functional

randomDouble [] [] d = do
    component <- MWC.withSystemRandom . MWC.asGenIO $ \gen -> genContVar d gen
    return $ Scalar component

randomDouble (d:ds) (s:size) distr = do
  tensors <- sequence [randomDouble ds size distr | _ <- [0 .. s - 1] ]
  return $  mergeScalars $ FiniteTensor (Covariant s [d]) $ ZipVector $ Boxed.fromList tensors

randomDouble _ _ _ = return $ Err "Indices and its sizes not compatible with structure of n-vector!"

{-| Generate n-vector with random integer components with given probability distribution.
The n-vector is wrapped in the IO monad. -}
{-| Available probability distributions: -}
{-| - Binomial : "Statistics.Distribution.Binomial" -}
{-| - Poisson : "Statistics.Distribution.Poisson" -}
{-| - Geometric : "Statistics.Distribution.Geometric" -}
{-| - Hypergeometric: "Statistics.Distribution.Hypergeometric" -}
randomInt :: (
    DiscreteGen d
  ) => String                -- ^ Indices names (one character per index)
    -> [Int]                   -- ^ Indices sizes
    -> d                     -- ^ Discrete probability distribution (as from "Statistics.Distribution")
    -> IO (VectorTensor Int)     -- ^ Generated n-vector

randomInt [] [] d = do
    component <- MWC.withSystemRandom . MWC.asGenIO $ \gen -> genDiscreteVar d gen
    return $ Scalar component

randomInt (d:ds) (s:size) distr = do
  tensors <- sequence [randomInt ds size distr | _ <- [0 .. s - 1] ]
  return $  mergeScalars $ FiniteTensor (Covariant s [d]) $ ZipVector $ Boxed.fromList tensors

randomInt _ _ _ = return $ Err "Indices and its sizes not compatible with structure of n-vector!"

{-| Generate n-vector with random real components with given probability distribution and given seed.
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
    ContGen d, Integral i2, PrimMonad m
  ) => String              -- ^ Index name (one character)
    -> [Int]                 -- ^ Number of elements
    -> d                   -- ^ Continuous probability distribution (as from "Statistics.Distribution")
    -> i2                  -- ^ Randomness seed
    -> m (VectorTensor Double) -- ^ Generated n-vector

randomDoubleSeed [] [] d seed = do
    gen <- MWC.initialize (Boxed.singleton $ fromIntegral seed)
    component <- genContVar d gen
    return $ Scalar component

randomDoubleSeed (d:ds) (s:size) distr seed = do
  tensors <- sequence [randomDoubleSeed ds size distr seed | _ <- [0 .. s - 1] ]
  return $  mergeScalars $ FiniteTensor (Covariant s [d]) $ ZipVector $ Boxed.fromList tensors

randomDoubleSeed _ _ _ _ = return $ Err "Indices and its sizes not compatible with structure of n-vector!"

{-| Generate n-vector with random integer components with given probability distribution and given seed.
The form is wrapped in a monad. -}
{-| Available probability distributions: -}
{-| - Binomial : "Statistics.Distribution.Binomial" -}
{-| - Poisson : "Statistics.Distribution.Poisson" -}
{-| - Geometric : "Statistics.Distribution.Geometric" -}
{-| - Hypergeometric: "Statistics.Distribution.Hypergeometric" -}
randomIntSeed :: (
    DiscreteGen d, Integral i2, PrimMonad m
  ) => String              -- ^ Index name (one character)
    -> [Int]                 -- ^ Number of elements
    -> d                   -- ^ Discrete probability distribution (as from "Statistics.Distribution")
    -> i2                  -- ^ Randomness seed
    -> m (VectorTensor Int)    -- ^ Generated n-vector

randomIntSeed [] [] d seed = do
    gen <- MWC.initialize (Boxed.singleton $ fromIntegral seed)
    component <- genDiscreteVar d gen
    return $ Scalar component

randomIntSeed (d:ds) (s:size) distr seed = do
  tensors <- sequence [randomIntSeed ds size distr seed | _ <- [0 .. s - 1] ]
  return $  mergeScalars $ FiniteTensor (Covariant s [d]) $ ZipVector $ Boxed.fromList tensors

randomIntSeed _ _ _ _ = return $ Err "Indices and its sizes not compatible with structure of n-vector!"


{-| 2-form representing a dot product -}
dot :: (
    Eq a, Show a, Num a, Bits a
  ) => String        -- ^ Indices names (one characted per index)
    -> Int           -- ^ Size of tensor (dot product is a square tensor)
    -> VectorTensor a  -- ^ Generated dot product

dot [i1,i2] size = fromIndices [i1,i2] [size,size] (\[i,j] -> if i == j then 1 else 0)
dot _ _ = Err "Indices and its sizes incompatible with dot product!"

{-| Tensor representing a cross product (Levi - Civita symbol). It also allows to compute a determinant of square matrix - determinant of matrix @M@ is a equal to length of cross product of all columns of @M@ -}
-- // TODO
cross :: (
    Eq a, Show a, Num a, Bits a
  ) => String        -- ^ Indices names (one characted per index)
    -> Int           -- ^ Size of tensor (dot product is a square tensor)
    -> VectorTensor a  -- ^ Generated dot product

cross [i,j,k] size =
  Tensor.fromIndices ([i],[size]) ([j,k],[size,size])
    (\[_] [_,_] -> 0)
cross _ _ = Err "Indices and its sizes incompatible with cross product!"
