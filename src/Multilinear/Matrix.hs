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
  fromIndices, 
  Multilinear.Matrix.const 
  --elm
) where

import           Multilinear.Generic.AsList
import           Multilinear.Index
import           Data.Hashable
import           Data.Bits


{-| Generate matrix as function of its indices -}
fromIndices :: (
    Eq i, Show i, Integral i, Ord i, Hashable i,
    Eq a, Show a, Integral a, Num a, Ord a, Hashable a, Bits a
  ) => String -> i -> i -> (i -> i -> a) -> Tensor i a

fromIndices [u,d] su sd f =
    Tensor (Contravariant su [u]) 
      [Tensor (Covariant sd [d]) 
        [Scalar $ f x y 
      | y <- [0 .. sd - 1] ] 
    | x <- [0 .. su - 1] ]
fromIndices _ _ _ _ = error "Indices and its sizes incompatible with matrix structure!"

{-| Generate matrix with all components equal to v |-}
const :: (
    Eq i, Show i, Integral i, Ord i, Hashable i,
    Eq a, Show a, Num a, Ord a, Hashable a, Bits a
  ) => String -> i -> i -> a -> Tensor i a

const [u,d] su sd v =
    Tensor (Contravariant su [u]) $
      replicate (fromIntegral su) $
        Tensor (Covariant sd [d]) $ 
          replicate (fromIntegral sd) $ Scalar v
      
const _ _ _ _ = error "Indices and its sizes incompatible with matrix structure!"

{-| Concise getter for a matrix -}
{-elm :: Integral i => Tensor i a -> i -> i -> a
elm m i1 i2 = scalarVal $ m ! i1 ! i2-}