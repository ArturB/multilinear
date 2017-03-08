{-|
Module      : Tensor
Description : Defines concise constructor and getter to operate on generic, arbitrary-dimensional tensor
Copyright   : (c) Artur M. Brodzki, 2017
License     : 3-clause BSD
Maintainer  : artur.brodzki@gmail.com
Stability   : experimental
Portability : Windows/POSIX

-}

{-# LANGUAGE GADTs  #-}
{-# LANGUAGE Strict #-}

module Multilinear.Tensor (
    tensor, el
) where

import           Multilinear.ListTensor

{-| Concise constructor for a tensor -}
tensor :: (Show i, Integral i) => (String,[i]) -> (String,[i]) -> ([i] -> [i] -> a) -> Tensor i a
tensor ([],[]) ([],[]) f = Scalar $ f [] []
tensor (u:us,s:size) d f =
    Tensor (Contravariant s [u]) [tensor (us,size) d (\uss dss -> f (x:uss) dss) | x <- [0 .. s - 1] ]
tensor u (d:ds,s:size) f =
    Tensor (Covariant s [d]) [tensor u (ds,size) (\uss dss -> f uss (x:dss)) | x <- [0 .. s - 1] ]
tensor us ds _ = error $ "Indices and its sizes incompatible, upper indices: " ++ show us ++", lower indices: " ++ show ds

{-| Concise getter for a tensor -}
el :: Integral i => Tensor i a -> [i] -> [i] -> a
el (Scalar x) [] [] = x
el (Err msg) _ _  = error msg
el t@(Tensor (Contravariant _ _) _) (u:us) ds = el (t ! u) us ds
el t@(Tensor (Covariant _ _) _) us (d:ds) = el (t ! d) us ds
el _ _ _ = error "Given indices are not compatible with tensor type!"