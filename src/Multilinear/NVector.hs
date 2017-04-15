{-|
Module      : Multilinear.NVector
Description : N-Vectors
Copyright   : (c) Artur M. Brodzki, 2017
License     : GPL-3
Maintainer  : artur.brodzki@gmail.com
Stability   : experimental
Portability : Windows/POSIX

-}

{-# LANGUAGE GADTs  #-}
{-# LANGUAGE Strict #-}
{-# OPTIONS_GHC #-}

module Multilinear.NVector (
  fromIndices,
  Multilinear.NVector.const
) where


import           Data.Bits
import           Multilinear.Generic.AsList
import           Multilinear.Index

{-| Generate n-vector as function of its indices -}
fromIndices :: (
    Eq i, Show i, Integral i,
    Eq a, Show a, Num a, Bits a
  ) => String -> [i] -> ([i] -> a) -> Tensor i a

fromIndices [] [] f = Scalar $ f []
fromIndices (d:ds) (s:size) f =
    Tensor (Contravariant s [d]) [fromIndices ds size (\dss -> f (x:dss)) | x <- [0 .. s - 1] ]
fromIndices _ _ _ = error "Indices and its sizes incompatible with n-vector structure!"

{-| Generate n-vector with all components equal to v -}
const :: (
    Eq i, Show i, Integral i,
    Eq a, Show a, Num a, Bits a
  ) => String -> [i] -> a -> Tensor i a

const [] [] v = Scalar v
const (d:ds) (s:size) v =
    Tensor (Contravariant s [d]) $ replicate (fromIntegral s) $ Multilinear.NVector.const ds size v
const _ _ _ = error "Indices and its sizes incompatible with n-vector structure!"

