{-|
Module      : Multilinear.NForm
Description : N-Forms, dot and cross product and determinant
Copyright   : (c) Artur M. Brodzki, 2017
License     : GLP-3
Maintainer  : artur.brodzki@gmail.com
Stability   : experimental
Portability : Windows/POSIX

This module provides convenient constructors that generates n-forms (tensors with n lower indices). 

-}

{-# LANGUAGE Strict, GADTs #-}
{-# OPTIONS_GHC #-}

module Multilinear.NForm (
  fromIndices, 
  Multilinear.NForm.const,
  dot, cross
) where

import           Multilinear.Generic.AsList
import           Multilinear.Index
import qualified Multilinear.Tensor as Tensor
import           Data.Bits

{-| Generate N-form as function of its indices -}
fromIndices :: (
    Eq i, Show i, Integral i,
    Eq a, Show a, Num a, Bits a
  ) => String      -- ^ Indices names (one characted per index)
    -> [i]         -- ^ Indices sizes
    -> ([i] -> a)  -- ^ Generator function
    -> Tensor i a  -- ^ Generated N-form

fromIndices [] [] f = Scalar $ f []
fromIndices (d:ds) (s:size) f =
    Tensor (Covariant s [d]) [fromIndices ds size (\dss -> f (x:dss)) | x <- [0 .. s - 1] ]
fromIndices _ _ _ = Err "Indices and its sizes incompatible with n-form structure!"

{-| Generate N-form with all components equal to @v@ -}
const :: (
    Eq i, Show i, Integral i,
    Eq a, Show a, Num a, Bits a
  ) => String      -- ^ Indices names (one characted per index)
    -> [i]         -- ^ Indices sizes
    -> a           -- ^ N-form elements value
    -> Tensor i a  -- ^ Generated N-form

const [] [] v = Scalar v
const (d:ds) (s:size) v =
    Tensor (Covariant s [d]) $ replicate (fromIntegral s) $ Multilinear.NForm.const ds size v
const _ _ _ = Err "Indices and its sizes incompatible with n-form structure!"

{-| 2-form representing a dot product -}
dot :: (
    Eq i, Show i, Integral i,
    Eq a, Show a, Num a, Bits a
  ) => String      -- ^ Indices names (one characted per index)
    -> i           -- ^ Size of tensor (dot product is a square tensor)
    -> Tensor i a  -- ^ Generated dot product

dot [i1,i2] size = fromIndices [i1,i2] [size,size] (\[i,j] -> if i == j then 1 else 0)
dot _ _ = Err "Indices and its sizes incompatible with dot product!"

{-| Tensor representing a cross product (Levi - Civita symbol). It also allows to compute a determinant of square matrix - determinant of matrix @M@ is a equal to length of cross product of all columns of @M@ -}
{- @TODO@ -}
cross :: (
    Eq i, Show i, Integral i,
    Eq a, Show a, Num a, Bits a
  ) => String      -- ^ Indices names (one characted per index)
    -> i           -- ^ Size of tensor (dot product is a square tensor)
    -> Tensor i a  -- ^ Generated dot product

cross [i,j,k] size = 
  Tensor.fromIndices ([i],[size]) ([j,k],[size,size]) 
    (\[_] [_,_] -> 0)
cross _ _ = Err "Indices and its sizes incompatible with cross product!"



