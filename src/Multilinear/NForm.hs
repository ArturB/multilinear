{-|
Module      : Multilinear.NForm
Description : N-Forms
Copyright   : (c) Artur M. Brodzki, 2017
License     : GLP-3
Maintainer  : artur.brodzki@gmail.com
Stability   : experimental
Portability : Windows/POSIX

-}

{-# LANGUAGE Strict, GADTs #-}
{-# OPTIONS_GHC #-}

module Multilinear.NForm (
  fromIndices, 
  Multilinear.NForm.const,
  --elnf
) where

import           Multilinear.Generic.AsList
import           Multilinear.Index
import           Data.Bits

{-| Generate N-form as function of its indices -}
fromIndices :: (
    Eq i, Show i, Integral i,
    Eq a, Show a, Num a, Bits a
  ) => String -> [i] -> ([i] -> a) -> Tensor i a

fromIndices [] [] f = Scalar $ f []
fromIndices (d:ds) (s:size) f =
    Tensor (Covariant s [d]) [fromIndices ds size (\dss -> f (x:dss)) | x <- [0 .. s - 1] ]
fromIndices _ _ _ = error "Indices and its sizes incompatible with n-form structure!"

{-| Generate N-form with all components equal to v -}
const :: (
    Eq i, Show i, Integral i,
    Eq a, Show a, Num a, Bits a
  ) => String -> [i] -> a -> Tensor i a

const [] [] v = Scalar v
const (d:ds) (s:size) v =
    Tensor (Covariant s [d]) $ replicate (fromIntegral s) $ Multilinear.NForm.const ds size v
const _ _ _ = error "Indices and its sizes incompatible with n-form structure!"

{-| Concise getter for a n-form -}
{-elnf :: Integral i => Tensor i a -> [i] -> a
elnf (Scalar x) [] = x
elnf (Err msg) _ = error msg
elnf t@(Tensor (Covariant _ _) _) (d:ds) = elnf (t ! d) ds
elnf _ _ = error "Given indices incompatible with n-form structure!"-}
