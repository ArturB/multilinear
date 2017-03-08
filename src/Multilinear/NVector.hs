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
  nvector, elnv
) where


import           Multilinear.ListTensor

{-| Concise constructor for a n-vector -}
nvector :: (Show i, Integral i) => String -> [i] -> ([i] -> a) -> Tensor i a
nvector [] [] f = Scalar $ f []
nvector (d:ds) (s:size) f =
    Tensor (Contravariant s [d]) [nvector ds size (\dss -> f (x:dss)) | x <- [0 .. s - 1] ]
nvector _ _ _ = error "Indices and its sizes incompatible with n-vector structure!"

{-| Concise getter for a n-vector -}
elnv :: Integral i => Tensor i a -> [i] -> a
elnv (Scalar x) [] = x
elnv (Err msg) _ = error msg
elnv t@(Tensor (Contravariant _ _) _) (d:ds) = elnv (t ! d) ds
elnv _ _ = error "Given indices incompatible with n-vector structure!"
