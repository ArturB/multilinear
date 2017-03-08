{-|
Module      : Tensor
Description : Tensor is a generic datatype to deal with linear algebra. May contain co- and contravariant indices as well.
Copyright   : (c) Artur M. Brodzki, 2017
License     : 3-clause BSD
Maintainer  : artur.brodzki@gmail.com
Stability   : experimental
Portability : Windows/POSIX

-}

{-# LANGUAGE GADTs  #-}
{-# LANGUAGE Strict #-}

module Multilinear.Tensor (
    tensor
) where

import           Multilinear.ListTensor


tensor :: (Show i, Integral i) => (String,[i]) -> (String,[i]) -> ([i] -> [i] -> a) -> Tensor i a
tensor ([],[]) ([],[]) f = Scalar $ f [] []
tensor (u:us,s:size) d f =
    Tensor (Contravariant s [u]) [tensor (us,size) d (\uss dss -> f (x:uss) dss) | x <- [0 .. s - 1] ]
tensor u (d:ds,s:size) f =
    Tensor (Covariant s [d]) [tensor u (ds,size) (\uss dss -> f uss (x:dss)) | x <- [0 .. s - 1] ]
tensor us ds _ = error $ "Indices and its sizes incompatible, upper indices: " ++ show us ++", lower indices: " ++ show ds

elt :: Tensor i a -> [i] -> [i] -> a
elt