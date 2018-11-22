{-|
Module      : Multilinear.Tensor
Description : Tensors constructors (finitely- or infinitely-dimensional)
Copyright   : (c) Artur M. Brodzki, 2018
License     : BSD3
Maintainer  : artur@brodzki.org
Stability   : experimental
Portability : Windows/POSIX

- This module provides convenient constructors that generate a arbitrary finitely- or infinitely-dimensional tensors. 
- Finitely-dimensional tensors provide much greater performance than inifitely-dimensional

-}

module Multilinear.Tensor (
  -- * Generators
  Multilinear.Tensor.generate
) where

import qualified Data.Vector                as Boxed
import qualified Data.Vector.Unboxed        as Unboxed
import           Multilinear.Generic
import           Multilinear.Index.Finite   as Finite

invalidIndices :: (String, [Int]) -> (String, [Int]) -> String
invalidIndices us ds = "Indices and its sizes incompatible, upper indices: " ++ show us ++", lower indices: " ++ show ds

{-| Generate tensor composed of other tensors -}
generate :: (
    Num a, Unboxed.Unbox a
    ) => (String,[Int])               -- ^ Upper indices names (one character per index) and its sizes
      -> (String,[Int])               -- ^ Lower indices names (one character per index) and its sizes
      -> ([Int] -> [Int] -> Tensor a) -- ^ Generator function (f [u1,u2,...] [d1,d2,...] returns a tensor element at t [u1,u2,...] [d1,d2,...])
      -> Tensor a                     -- ^ Generated tensor

-- If no indices are given, generate a tensor by using generator function
generate ([],[]) ([],[]) f = f [] []

-- If many indices are given, first generate upper indices recursively from indices list
generate (u:us,s:size) d f =
    FiniteTensor (Contravariant s [u]) $ Boxed.generate s (\x -> generate (us,size) d (\uss dss -> f (x:uss) dss) )

-- After upper indices, generate lower indices recursively from indices list
generate u (d:ds,s:size) f =
    FiniteTensor (Covariant s [d]) $ Boxed.generate s (\x -> generate u (ds,size) (\uss dss -> f uss (x:dss)) )

-- If there are indices without size or sizes without names, throw an error
generate us ds _ = error $ invalidIndices us ds
