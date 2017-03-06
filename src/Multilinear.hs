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

{-# LANGUAGE Strict, GADTs #-}
{-# OPTIONS_GHC -O2 #-}

module Multilinear (
    -- * Classes
    Multilinear(..),
    -- * Re-export other library modules
    module X
) where

-- Re-export other library modules
import Multilinear.Index as X
import Multilinear.Covector as X
import Multilinear.Vector as X
import Multilinear.Tensor as X

{-| Multidimensional arrays are trated as tensors - multilinear maps. -}
class Num t=> Multilinear t where 
    {-| Add scalar left -}
    infixl 7 .+
    (.+) :: Num a => a -> t -> t

    {-| Subtract scalar left -}
    infixl 7 .-
    (.-) :: Num a => a -> t -> t

    {-| Multiply by scalar left-}
    infixl 8 .*
    (.*) :: Num a => a -> t -> t

    {-| Add scalar right -}
    infixl 7 +.
    (+.) :: Num a => t -> a -> t

    {-| Subtract scalar right -}
    infixl 7 -.
    (-.) :: Num a => t -> a -> t

    {-| Multiply by scalar right-}
    infixl 8 *.
    (*.) :: Num a => t -> a -> t

    {-| Tensor order (contravariant, covariant) -}
    order :: t -> (Int,Int)

    {-| List of tensor indices -}
    indices :: t -> [TIndex]

    {-| Rename tensor index -}
    rename :: t -> Char -> Char -> t

    {-| Switch all indices type -}
    transpose :: t -> t

    {-| Inverse tensor -}
    inverse :: t -> t

    {-| Concatenation of two tensor with given index or by creating a new one -}
    concat ::  Char -> t -> t -> t

    {-| Check if tensors are equivalent (are of the same type and size) -}
    equiv :: t -> t -> Bool

    {-| Number of tensor elements -}
    elems :: t -> Int
    

