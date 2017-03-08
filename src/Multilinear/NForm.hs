{-|
Module      : Multilinear.NForm
Description : 
Copyright   : (c) Artur M. Brodzki, 2017
License     : 3-clause BSD
Maintainer  : artur.brodzki@gmail.com
Stability   : experimental
Portability : Windows/POSIX

-}

{-# LANGUAGE Strict, GADTs #-}
{-# OPTIONS_GHC #-}

module Multilinear.NForm (
  nform, elnf
) where

import           Multilinear.ListTensor

{-| Concise constructor for a n-form -}
nform :: (Show i, Integral i) => String -> [i] -> ([i] -> a) -> Tensor i a
nform [] [] f = Scalar $ f []
nform (d:ds) (s:size) f =
    Tensor (Covariant s [d]) [nform ds size (\dss -> f (x:dss)) | x <- [0 .. s - 1] ]
nform _ _ _ = error "Indices and its sizes incompatible with n-form structure!"

{-| Concise getter for a n-form -}
elnf :: Integral i => Tensor i a -> [i] -> a
elnf (Scalar x) [] = x
elnf (Err msg) _ = error msg
elnf t@(Tensor (Covariant _ _) _) (d:ds) = elnf (t ! d) ds
elnf _ _ = error "Given indices incompatible with n-form structure!"
