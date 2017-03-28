{-|
Module      : Multilinear.Operations
Description : Defines a Multilinear typeclass which specifies operations each tensor should provide
Copyright   : (c) Artur M. Brodzki, 2017
License     : 3-clause BSD
Maintainer  : artur.brodzki@gmail.com
Stability   : experimental
Portability : Windows/POSIX

-}

module Multilinear.Operations (
  Multilinear(..)
) where

import           Multilinear.Index

{-| Multidimensional arrays are trated as tensors - multilinear maps. -}
class (
  Eq (t i a),         -- Eq and Show are superclasses of Num
  Show (t i a),       -- Eq and Show are superclasses of Num 
  Num (t i a),        -- Tensors may be added, subtracted and multiplicated
  Fractional (t i a), -- Multiplicating has its inverse
  Floating (t i a),   -- Tensors may be de-liearinzed by common non-linear functions
  Integral i,         -- Indices of tensors must be of integral type to sum them up in finite manner
  Functor (t i)       -- Tensors are multi-dimensional arrays and must be functors
  ) => Multilinear t i a where

    {-| Recursive indexing -}
    infixl 9 !
    (!) :: t i a -> i -> t i a

    {-| Add scalar left -}
    infixl 7 .+
    (.+) :: a -> t i a -> t i a

    {-| Subtract scalar left -}
    infixl 7 .-
    (.-) :: a -> t i a -> t i a

    {-| Multiply by scalar left-}
    infixl 8 .*
    (.*) :: a -> t i a -> t i a

    {-| Divide by scalar left -}
    (./) :: a -> t i a -> t i a

    {-| Add scalar right -}
    infixl 7 +.
    (+.) :: t i a -> a -> t i a

    {-| Subtract scalar right -}
    infixl 7 -.
    (-.) :: t i a -> a -> t i a

    {-| Multiply by scalar right-}
    infixl 8 *.
    (*.) :: t i a -> a -> t i a

    {-| Divide by scalar right -}
    (/.) :: t i a -> a -> t i a

    {-| Tensor order (contravariant, covariant) -}
    order :: t i a -> (Int,Int)

    {-| List of tensor indices -}
    indices :: t i a -> [TIndex i]

    {-| Rename tensor index -}
    rename :: t i a -> String -> String -> t i a

    {-| Switch all indices type -}
    transpose :: t i a -> t i a

    {-| Inverse tensor -}
    inverse :: t i a -> t i a

    {-| Concatenation of two tensor with given index or by creating a new one -}
    concat ::  Char -> t i a -> t i a -> t i a

    {-| Check if tensors are equivalent (are of the same type and size) -}
    equiv :: t i a -> t i a -> Bool

    {-| Number of tensor elements -}
    elems :: t i a -> i


