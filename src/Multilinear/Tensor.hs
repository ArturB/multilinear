{-|
Module      : Tensor
Description : Tensors
Copyright   : (c) Artur M. Brodzki, 2017
License     : GPL-3
Maintainer  : artur.brodzki@gmail.com
Stability   : experimental
Portability : Windows/POSIX

-}

{-# LANGUAGE GADTs  #-}
{-# LANGUAGE Strict #-}

module Multilinear.Tensor (
    fromIndices,
    Multilinear.Tensor.const
    --el
) where

import           Multilinear.Generic.AsList
import           Multilinear.Index
import           Data.Bits

{-| Generate tensor as functions of its indices -}
fromIndices :: (
    Eq i, Show i, Integral i,
    Eq a, Show a, Num a, Bits a
    ) => (String,[i]) -> (String,[i]) -> ([i] -> [i] -> a) -> Tensor i a

fromIndices ([],[]) ([],[]) f = Scalar $ f [] []
fromIndices (u:us,s:size) d f =
    Tensor (Contravariant s [u]) [fromIndices (us,size) d (\uss dss -> f (x:uss) dss) | x <- [0 .. s - 1] ]
fromIndices u (d:ds,s:size) f =
    Tensor (Covariant s [d]) [fromIndices u (ds,size) (\uss dss -> f uss (x:dss)) | x <- [0 .. s - 1] ]
fromIndices us ds _ = error $ "Indices and its sizes incompatible, upper indices: " ++ show us ++", lower indices: " ++ show ds

{-| Generate tensor with all components equal to v -}
const :: (
    Eq i, Show i, Integral i,
    Eq a, Show a, Num a, Bits a
    ) => (String,[i]) -> (String,[i]) -> a -> Tensor i a

const ([],[]) ([],[]) v = Scalar v
const (u:us,s:size) d v =
    Tensor (Contravariant s [u]) $ replicate (fromIntegral s) $ Multilinear.Tensor.const (us,size) d v
const u (d:ds,s:size) v =
    Tensor (    Covariant s [d]) $ replicate (fromIntegral s) $ Multilinear.Tensor.const u (ds,size) v
const us ds _ = error $ "Indices and its sizes incompatible, upper indices: " ++ show us ++", lower indices: " ++ show ds



{-| Concise getter for a tensor -}
{-el :: Integral i => Tensor i a -> [i] -> [i] -> a
el (Scalar x) [] [] = x
el (Err msg) _ _  = error msg
el t@(Tensor (Contravariant _ _) _) (u:us) ds = el (t ! u) us ds
el t@(Tensor (Covariant _ _) _) us (d:ds) = el (t ! d) us ds
el _ _ _ = error "Given indices are not compatible with tensor type!"-}