{-|
Module      : Covector
Description : Implements a Covector datatype - a tensor with only covariant indices, especially 1-covector: linear functional.
Copyright   : (c) Artur M. Brodzki, 2017
License     : 3-clause BSD
Maintainer  : artur.brodzki@gmail.com
Stability   : experimental
Portability : Windows/POSIX

-}

{-# LANGUAGE GADTs  #-}
{-# LANGUAGE Strict #-}

module Multilinear.Form (
  form, elf
) where

import           Multilinear.ListTensor

{-| Concise constructor for a linear form -}
form :: (Show i, Integral i) => String -> i -> (i -> a) -> Tensor i a
form [d] s f =
    Tensor (Covariant s [d]) [Scalar $ f x | x <- [0 .. s - 1] ]
form _ _ _ = error "Indices and its sizes not compatible with structure of 1-form!"

{-| Concise getter for a linear form -}
elf :: Integral i => Tensor i a -> i -> a
elf (Err msg) _  = error msg
elf t@(Tensor (Covariant _ _) _) u = scalarVal $ t ! u
elf _ _ = error "Given indices are not compatible with 1-form structure!"
