{-|
Module      : Vector
Description : Vector
Copyright   : (c) Artur M. Brodzki, 2017
License     : GPL-3
Maintainer  : artur.brodzki@gmail.com
Stability   : experimental
Portability : Windows/POSIX



-}

{-# LANGUAGE Strict, GADTs #-}

module Multilinear.Vector (
  fromIndices, 
  Multilinear.Vector.const
  --elv
) where

import           Multilinear.Generic.AsList
import           Multilinear.Index
import           Data.Bits

{-| Generate vector as function of its indices -}
fromIndices :: (
    Eq i, Show i, Integral i,
    Eq a, Show a, Num a, Bits a
  ) => String -> i -> (i -> a) -> Tensor i a

fromIndices [u] s f =
    Tensor (Contravariant s [u]) [Scalar $ f x | x <- [0 .. s - 1] ]
fromIndices _ _ _ = error "Indices and its sizes not compatible with structure of vector!"

{-| Generate vector with all components equal to v -}
const :: (
    Eq i, Show i, Integral i,
    Eq a, Show a, Num a, Bits a
  ) => String -> i -> a -> Tensor i a

const [u] s v =
    Tensor (Contravariant s [u]) $ replicate (fromIntegral s) $ Scalar v
const _ _ _ = error "Indices and its sizes not compatible with structure of vector!"


{-| Concise getter for a vector -}
{-elv :: Integral i => Tensor i a -> i -> a
elv (Err msg) _  = error msg
elv t@(Tensor (Contravariant _ _) _) u = scalarVal $ t ! u
elv _ _ = error "Given indices are not compatible with vector structure!"-}
