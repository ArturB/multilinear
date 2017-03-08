{-|
Module      : Multilinear.Matrix
Description : 
Copyright   : (c) Artur M. Brodzki, 2017
License     : 3-clause BSD
Maintainer  : artur.brodzki@gmail.com
Stability   : experimental
Portability : Windows/POSIX

-}

{-# LANGUAGE Strict, GADTs #-}
{-# OPTIONS_GHC #-}

module Multilinear.Matrix (
  matrix, elm
) where

import           Multilinear.ListTensor


{-| Concise constructor for a matrix -}
matrix :: (Show i, Integral i) => String -> i -> i -> (i -> i -> a) -> Tensor i a
matrix [u,d] su sd f =
    Tensor (Contravariant su [u]) 
      [Tensor (Covariant sd [d]) 
        [Scalar $ f x y 
      | y <- [0 .. sd - 1] ] 
    | x <- [0 .. su - 1] ]
matrix _ _ _ _ = error "Indices and its sizes incompatible with matrix structure!"

{-| Concise getter for a matrix -}
elm :: Integral i => Tensor i a -> i -> i -> a
elm m i1 i2 = scalarVal $ m ! i1 ! i2