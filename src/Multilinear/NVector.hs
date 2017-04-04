{-|
Module      : Multilinear.NVector
Description : 
Copyright   : (c) Artur M. Brodzki, 2017
License     : 3-clause BSD
Maintainer  : artur.brodzki@gmail.com
Stability   : experimental
Portability : Windows/POSIX

-}

{-# LANGUAGE Strict, GADTs #-}
{-# OPTIONS_GHC #-}

module Multilinear.NVector (
  fromIndices, 
  Multilinear.NVector.const
  --elnv
) where


import           Multilinear.Generic.AsList
import           Multilinear.Index
import           Data.Hashable
import           Data.Bits

{-| Generate n-vector as function of its indices -}
fromIndices :: (
    Eq i, Show i, Integral i, Ord i, Hashable i,
    Eq a, Show a, Num a, Ord a, Hashable a, Bits a
  ) => String -> [i] -> ([i] -> a) -> Tensor i a

fromIndices [] [] f = Scalar $ f []
fromIndices (d:ds) (s:size) f =
    Tensor (Contravariant s [d]) [fromIndices ds size (\dss -> f (x:dss)) | x <- [0 .. s - 1] ]
fromIndices _ _ _ = error "Indices and its sizes incompatible with n-vector structure!"

{-| Generate n-vector with all components equal to v -}
const :: (
    Eq i, Show i, Integral i, Ord i, Hashable i,
    Eq a, Show a, Num a, Ord a, Hashable a, Bits a
  ) => String -> [i] -> a -> Tensor i a

const [] [] v = Scalar v
const (d:ds) (s:size) v =
    Tensor (Contravariant s [d]) $ replicate (fromIntegral s) $ Multilinear.NVector.const ds size v
const _ _ _ = error "Indices and its sizes incompatible with n-vector structure!"

{-| Concise getter for a n-vector -}
{-elnv :: Integral i => Tensor i a -> [i] -> a
elnv (Scalar x) [] = x
elnv (Err msg) _ = error msg
elnv t@(Tensor (Contravariant _ _) _) (d:ds) = elnv (t ! d) ds
elnv _ _ = error "Given indices incompatible with n-vector structure!"-}
