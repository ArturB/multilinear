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

-- | Set of sample tensors for testing. 
-- | All vectors, forms and matrices has indices from set [i,j,k] 
-- | and sizes compatible with each other, suitable for all (+,-,*) operator. 
-- | We have 33 tensors here, which allows to 33^2 circ. 1000 possible test cases for binary operators. 
tensors :: [Tensor Double]
tensors = [
    -- Scalars
    Scalar (-1.0)
  , Scalar 0.0
  , Scalar 1.0
    -- Vectors with i,j,k indices
  , Vector.fromIndices "i" iS fromIntegral
  , Vector.fromIndices "j" jS (\x -> fromIntegral x - 5.0)
  , Vector.fromIndices "k" kS fromIntegral
    -- Functional with i,j,k indices - can be contracted with vectors above or matrices below
  , Form.fromIndices "i" iS fromIntegral
  , Form.fromIndices "j" jS (\x -> fromIntegral x - 5.0)
  , Form.fromIndices "k" kS fromIntegral
    -- Matrices with a,b indices
  , Matrix.fromIndices "ab" aS bS (\i j -> fromIntegral i + fromIntegral j)
  , Matrix.fromIndices "ab" aS bS (\i j -> 5 * fromIntegral i - fromIntegral j)
  , Matrix.fromIndices "ab" aS bS (\_ _ -> 0.0)
    -- The same matrices as above, but with changed indices order
  , Matrix.fromIndices "ab" aS bS (\i j -> fromIntegral i + fromIntegral j)     |>>> "a"
  , Matrix.fromIndices "ab" aS bS (\i j -> 5 * fromIntegral i - fromIntegral j) |>>> "a"
  , Matrix.fromIndices "ab" aS bS (\_ _ -> 0.0)                                 |>>> "a"
    -- Matrices with i,j,k indices and the same indices sizes as for vectors above
  , Matrix.fromIndices "ij" iS jS (\i j -> fromIntegral i + fromIntegral j)
  , Matrix.fromIndices "kj" kS jS (\i j -> fromIntegral i + fromIntegral j)
  , Matrix.fromIndices "ik" iS kS (\i j -> fromIntegral i + fromIntegral j)
    -- The same matrices as above, but with changed indices order
  , Matrix.fromIndices "ij" iS jS (\i j -> fromIntegral i + fromIntegral j) |>>> "i"
  , Matrix.fromIndices "kj" kS jS (\i j -> fromIntegral i + fromIntegral j) |>>> "k"
  , Matrix.fromIndices "ik" iS kS (\i j -> fromIntegral i + fromIntegral j) |>>> "i"
    -- Matrices with one i,j,k index and one a,b index
  , Matrix.fromIndices "ja" jS aS (\i j -> fromIntegral i + fromIntegral j)
  , Matrix.fromIndices "ak" aS kS (\i j -> fromIntegral i + fromIntegral j)
  , Matrix.fromIndices "ai" aS iS (\i j -> fromIntegral i + fromIntegral j)
  , Matrix.fromIndices "jb" jS bS (\i j -> fromIntegral i + fromIntegral j)
  , Matrix.fromIndices "bk" bS kS (\i j -> fromIntegral i + fromIntegral j)
  , Matrix.fromIndices "bi" bS iS (\i j -> fromIntegral i + fromIntegral j)
    -- The same matrices as above, but with changed indices order
  , Matrix.fromIndices "ja" jS aS (\i j -> fromIntegral i + fromIntegral j) |>>> "j"
  , Matrix.fromIndices "ak" aS kS (\i j -> fromIntegral i + fromIntegral j) |>>> "a"
  , Matrix.fromIndices "ai" aS iS (\i j -> fromIntegral i + fromIntegral j) |>>> "a"
  , Matrix.fromIndices "jb" jS bS (\i j -> fromIntegral i + fromIntegral j) |>>> "j"
  , Matrix.fromIndices "bk" bS kS (\i j -> fromIntegral i + fromIntegral j) |>>> "b"
  , Matrix.fromIndices "bi" bS iS (\i j -> fromIntegral i + fromIntegral j) |>>> "b"
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
