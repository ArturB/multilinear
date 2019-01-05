{-|
Module      : Test.QuickCheck.Multilinear.Generic.MultiCore
Description : QucikCheck instances of MultiCore tensor
Copyright   : (c) Artur M. Brodzki, 2018
License     : BSD3
Maintainer  : artur@brodzki.org
Stability   : experimental
Portability : Windows/POSIX

-}

module Test.QuickCheck.Multilinear.Generic.MultiCore (
    Arbitrary
) where

import qualified Multilinear.Form              as Form
import           Multilinear.Generic.MultiCore
import qualified Multilinear.Vector            as Vector
import           Test.QuickCheck

-- | Sizes of indices used in Arbitrary Tensor instance
aS :: Int
aS = 12
bS :: Int
bS = 12
iS :: Int
iS = 10
jS :: Int
jS = 15
kS :: Int
kS = 10

-- | Set of three Scalars, that can be used for building more complex tensors
scalars :: [Tensor Double]
scalars = [
  -- Scalars
    Scalar (-1.0)
  , Scalar 0.0
  , Scalar 1.0
  ]

-- | Set of 5 simple Scalars and 1D tensors for testing with only upper indices
-- | We use set of a,b,i,j,k indices for further building more complex tensors sets
tensors1DUpper :: [Tensor Double]
tensors1DUpper = [
    -- Vectors with a,b,i,j,k indices
    Vector.fromIndices "a" aS $ sin . fromIntegral
  , Vector.fromIndices "b" bS $ cos . fromIntegral
  , Vector.fromIndices "i" iS $ exp . fromIntegral
  , Vector.fromIndices "j" jS $ cosh . fromIntegral
  , Vector.fromIndices "k" kS $ tanh . fromIntegral
    ]

-- | Set of 5 simple Scalars and 1D tensors for testing with only lower indices
-- | We use set of a,b,i,j,k indices for further building more complex tensors sets
tensors1DLower :: [Tensor Double]
tensors1DLower = [
    -- Functional with a,b,i,j,k indices - can be contracted with vectors above or matrices below
    Form.fromIndices "a" aS $ sin . fromIntegral
  , Form.fromIndices "b" bS $ cos . fromIntegral
  , Form.fromIndices "i" iS $ exp . fromIntegral
  , Form.fromIndices "j" jS $ cosh . fromIntegral
  , Form.fromIndices "k" kS $ tanh . fromIntegral
    ]

-- | List sum of scalars and upper and lower indices simple tensors
-- | List contains 18 tensors in total
tensors1D :: [Tensor Double]
tensors1D = scalars ++ tensors1DUpper ++ tensors1DLower

{-| More complex (up to 3D) tensors, built as sums and differences of tensor products of all pairs from tensors list above
    List contains 18^3 = 5832 tensors in total -}

tensors3D :: [Tensor Double]
tensors3D = pure (*) <*> ts <*> tensors1D
  where ts = pure (*) <*> tensors1D <*> tensors1D

-- | Arbitrary random generating instance of Tensor Double
-- | Simply choose a tensot from tensors list above
instance Arbitrary (Tensor Double) where
    arbitrary = elements tensors3D
