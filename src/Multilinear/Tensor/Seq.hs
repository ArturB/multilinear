{-|
Module      : Multilinear.Tensor.Seq
Description : 
Copyright   : (c) Artur M. Brodzki, 2017
License     : 3-clause BSD
Maintainer  : artur.brodzki@gmail.com
Stability   : experimental
Portability : Windows/POSIX

-}

{-# LANGUAGE Strict, GADTs #-}
{-# OPTIONS_GHC #-}

module Multilinear.Tensor.Seq (
  
) where

{-import           Multilinear.ListTensor

{-| Concise constructor for a tensor memoized sequence -}
tensorseq :: (Show i, Integral i) => (String,[i]) -> (String,[i]) -> (String,i) -> ([i] -> [i] -> i -> a) -> Tensor i a
tensorseq ([],[]) ([],[]) ([i],is) f = 
  Tensor (Indifferent is [i]) [Scalar $ f [] [] x | x <- [0 .. is - 1] ]
tensorseq (u:us,s:size) d (i,is) f =
    Tensor (Contravariant s [u]) [tensorseq (us,size) d (i,is) (\uss dss -> f (x:uss) dss) | x <- [0 .. s - 1] ]
tensorseq u (d:ds,s:size) (i,is) f =
    Tensor (Covariant s [d]) [tensorseq u (ds,size) (i,is) (\uss dss -> f uss (x:dss)) | x <- [0 .. s - 1] ]
tensorseq us ds _ _ = error $ "Indices and its sizes incompatible, upper indices: " ++ show us ++", lower indices: " ++ show ds

{-| Concise getter for a tensor memoized sequence -}
elseq :: Integral i => Tensor i a -> [i] -> [i] -> i -> a
elseq (Scalar x) [] [] _ = x
elseq (Err msg) _ _ _ = error msg
elseq t@(Tensor (Contravariant _ _) _) (u:us) ds is = elseq (t ! u) us ds is
elseq t@(Tensor (Covariant _ _) _) us (d:ds) is = elseq (t ! d) us ds is
elseq t@(Tensor (Indifferent _ _) _) us ds is = elseq (t ! is) us ds is
elseq _ _ _ _ = error "Given indices are not compatible with tensor type!"
-}