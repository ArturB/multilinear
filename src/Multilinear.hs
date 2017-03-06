{-|
Module      : Multilinear
Description : Provides efficient and generic implementation of linear algebra operation using Ricci - Einstein tensor formalism
Copyright   : (c) Artur M. Brodzki, 2017
License     : 3-clause BSD
Maintainer  : artur.brodzki@gmail.com
Stability   : experimental
Portability : Windows/POSIX

Defines main tyclasses to deal with multilinear algebra and re-exports content of other library modules. 

-}

{-# LANGUAGE Strict, FlexibleInstances, FlexibleContexts, GADTs #-}
{-# OPTIONS_GHC -O2 #-}

module Multilinear (
    -- * Classes
    Multilinear(..),
    Tensor(..),
    -- * Other library modules
    module X
) where

import Multilinear.Index as X
import Multilinear.Covector as X
import Multilinear.Vector as X
import Multilinear.Tensor as X

{-| MULTILINEAR CLASS - TENSOR DEFINED AS MULTILINEAR MAP -}
class Multilinear t where 
    {-| Tensor sum -}
    infixl 7 !+
    (!+) :: t -> t -> t

    {-| Tensor difference -}
    infixl 7 !-
    (!-) :: t -> t -> t

    {-| Tensor product satisfying Einstein summation convention -}
    infixl 8 !*
    (!*) :: t -> t -> t

{-| TENSOR CLASS - TENSOR AS MULTIDIMENSIONAL ARRAY -}
class Num t => Tensor t where
    {-| Generate tensor elements as a function of indices -}
    generate :: TIndex -> (Int -> t) -> t
    {-| generate rank tensor from a list -}
    fromList :: TIndex -> [a] -> t
    {-| Tensor order (contravariant, covariant) -}
    order :: t -> (Int,Int)
    {-| Number of tensor elements -}
    elems :: t -> Int
    {-| List of tensor indices -}
    indices :: t -> [TIndex]
    {-| Rename tensor index -}
    rename :: t -> String -> String -> t
    {-| Check if tensors are equivalent (are of the same type and size) -}
    equiv :: t -> t -> Bool
    {-| switch all indices type -}
    transpose :: t -> t
    {-| switch only one index type -}
    transpose1 :: t -> t
    {-| concatenation of two tensor with given index or by creating a new one -}
    concat ::  TIndex -> t -> t -> t


{-}
instance Num b => Num (a -> b) where
    (f1 + f2) x = f1 x + f2 x
    (f1 * f2) x = f1 x * f2 x
    (abs f) x = abs (f x)
    (signum f) x = signum (f x)
    (negate f) x = negate (f x)
-}


