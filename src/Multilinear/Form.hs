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
  Multilinear.Form.const 
  --elf
) where

import           Multilinear.Generic.AsList
import           Multilinear.Index
import           Data.Hashable
import           Data.Bits

{-| Generate form as function of indices |-}
fromIndices :: (
    Eq i, Show i, Integral i, Ord i, Hashable i,
    Eq a, Show a, Num a, Ord a, Hashable a, Bits a
  ) => String -> i -> (i -> a) -> Tensor i a

fromIndices [d] s f =
    Tensor (Covariant s [d]) [Scalar $ f x | x <- [0 .. s - 1] ]
fromIndices _ _ _ = Err "Indices and its sizes not compatible with structure of 1-form!"

const :: (
    Eq i, Show i, Integral i, Ord i, Hashable i,
    Eq a, Show a, Num a, Ord a, Hashable a, Bits a
  ) => String -> i -> a -> Tensor i a

{-| Generate form with all s components equal to v |-}
const [d] s v = 
    Tensor (Covariant s [d]) $ replicate (fromIntegral s) (Scalar v)
const _ _ _ = Err "Indices and its sizes not compatible with structure of 1-form!"

{-| Concise getter for a linear form -}
{-
elf :: Integral i => Tensor i a -> i -> a
elf (Err msg) _  = error msg
elf t@(Tensor (Covariant _ _) _) u = scalarVal $ t ! u
elf _ _ = error "Given indices are not compatible with 1-form structure!"
-}