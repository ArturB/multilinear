{-|
Module      : Test.QuickCheck.Multilinear
Description : QucikCheck instances of Multilinear library
Copyright   : (c) Artur M. Brodzki, 2018
License     : BSD3
Maintainer  : artur@brodzki.org
Stability   : experimental
Portability : Windows/POSIX

-}

module Test.QuickCheck.Multilinear (
    Arbitrary
) where

import           Multilinear.Class
import qualified Multilinear.Form         as Form
import           Multilinear.Generic
import qualified Multilinear.Matrix       as Matrix
import qualified Multilinear.Vector       as Vector
import           Test.QuickCheck

-- | Set of sample tensors for testing. 
-- | All vectors, forms and matrices has indices from set [i,j,k] 
-- | and sizes compatible with each other, suitable for all (+,-,*) operator. 
tensors :: [Tensor Double]
tensors = [
    -- Scalars
    Scalar (-1.0)
  , Scalar 0.0
  , Scalar 1.0
    -- Vectors with i,j,k indices
  , Vector.fromIndices "i" 10 fromIntegral
  , Vector.fromIndices "j" 15 (\x -> fromIntegral x - 5.0)
  , Vector.fromIndices "k" 10 fromIntegral
    -- Functional with i,j,k indices
  , Form.fromIndices "i" 10 fromIntegral
  , Form.fromIndices "j" 15 (\x -> fromIntegral x - 5.0)
  , Form.fromIndices "k" 10 fromIntegral
    -- Matrices with a,b indices
  , Matrix.fromIndices "ab" 12 12 (\i j -> fromIntegral i + fromIntegral j)
  , Matrix.fromIndices "ab" 12 12 (\i j -> 5 * fromIntegral i - fromIntegral j)
  , Matrix.fromIndices "ab" 12 12 (\_ _ -> 0.0)
    -- The same matrices as aboe, but with changed indices order
  , Matrix.fromIndices "ab" 12 12 (\i j -> fromIntegral i + fromIntegral j)     |>>> "a"
  , Matrix.fromIndices "ab" 12 12 (\i j -> 5 * fromIntegral i - fromIntegral j) |>>> "a"
  , Matrix.fromIndices "ab" 12 12 (\_ _ -> 0.0)                                 |>>> "a"
    -- Matrices with i,j,k indices and the same indices sizes as for vectors above
  , Matrix.fromIndices "ij" 10 15 (\i j -> fromIntegral i + fromIntegral j)
  , Matrix.fromIndices "kj" 10 15 (\i j -> fromIntegral i + fromIntegral j)
  , Matrix.fromIndices "ik" 10 10 (\i j -> fromIntegral i + fromIntegral j)
    -- The same matrices as aboe, but with changed indices order
  , Matrix.fromIndices "ij" 10 15 (\i j -> fromIntegral i + fromIntegral j) |>>> "i"
  , Matrix.fromIndices "kj" 10 15 (\i j -> fromIntegral i + fromIntegral j) |>>> "k"
  , Matrix.fromIndices "ik" 10 10 (\i j -> fromIntegral i + fromIntegral j) |>>> "i"
    ]

{-
-- | Second set of tensors; its indices have sizes incompatible with indices of tensors above. 
-- | Multiplicating, adding and so on of tensors2 with tensors assumes an error. 
tensors2 :: [Tensor Double]
tensors2 = [
    -- Matrices with i,j,k indices and different indices sizes as for vectors above
    Matrix.fromIndices "ij" 11 16 (\i j -> fromIntegral i + fromIntegral j)
  , Matrix.fromIndices "kj" 11 16 (\i j -> fromIntegral i + fromIntegral j)
  , Matrix.fromIndices "ik" 11 11 (\i j -> fromIntegral i + fromIntegral j)
    -- The same matrices as above but with changed indices order
  , Matrix.fromIndices "ij" 11 16 (\i j -> fromIntegral i + fromIntegral j) |>>> "i"
  , Matrix.fromIndices "kj" 11 16 (\i j -> fromIntegral i + fromIntegral j) |>>> "k"
  , Matrix.fromIndices "ik" 11 11 (\i j -> fromIntegral i + fromIntegral j) |>>> "i"
 ]
-}

-- | Arbitrary random generating instance of Tensor Double
-- | Simply choose a tensot from tensors list above
instance Arbitrary (Tensor Double) where
    arbitrary = elements tensors
