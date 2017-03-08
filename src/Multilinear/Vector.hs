{-|
Module      : Vector
Description : Vector is a tensor with only contravariant indices. It refleclts a common understanding of vector as some point in space. 
Copyright   : (c) Artur M. Brodzki, 2017
License     : 3-clause BSD
Maintainer  : artur.brodzki@gmail.com
Stability   : experimental
Portability : Windows/POSIX

-}

{-# LANGUAGE Strict, GADTs #-}

module Multilinear.Vector (
  vector, elv
) where

import           Multilinear.ListTensor

{-| Concise constructor for a vector -}
vector :: (Show i, Integral i) => String -> i -> (i -> a) -> Tensor i a
vector [u] s f =
    Tensor (Contravariant s [u]) [Scalar $ f x | x <- [0 .. s - 1] ]
vector _ _ _ = error "Indices and its sizes not compatible with structure of vector!"

{-| Concise getter for a vector -}
elv :: Integral i => Tensor i a -> i -> a
elv (Err msg) _  = error msg
elv t@(Tensor (Contravariant _ _) _) u = scalarVal $ t ! u
elv _ _ = error "Given indices are not compatible with vector structure!"
