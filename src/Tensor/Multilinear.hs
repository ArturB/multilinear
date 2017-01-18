-----------------------------------------------------------------------------
--
-- Package     :  Tensor
-- Module      :  Tensor
-- Description :  Defines main operations that every Tensor instance should provide
-- Author      :  Artur M. Brodzki, Warsaw 2016
-----------------------------------------------------------------------------

{-# LANGUAGE Strict #-}
{-# OPTIONS_GHC -O2 #-}

module Tensor.Multilinear (
    Multilinear(..)
) where

import Tensor.Index

{- TENSOR CLASS - TENSOR DEFINED AS MULTILINEAR MAP -}

class Functor t => Multilinear t where
    -- Indexing
    (!!) :: t a -> Int -> t a
    -- Projection
    --(!!) :: t a -> [(String,Int)] -> Maybe (t a)
    -- Tensor product with Einstein summation convention
    infixl 8 !*
    (!*) :: Num a => t a -> t a -> t a
    -- Generate tensor elements as a function of indices
    generate :: TIndex -> (Int -> t a) -> t a
    -- generate rank tensor from a list
    fromList :: TIndex -> [a] -> t a
    -- Tensor order (contravariant, covariant)
    order :: t a -> (Int,Int)
    -- Number of tensor elements
    elems :: t a -> Int
    -- List of tensor indices
    indices :: t a -> [TIndex]
    -- Rename tensor index
    rename :: t a -> String -> String -> t a
    -- Check if tensors are equivalent (are of the same type and size)
    equiv :: t a -> t a -> Bool
    -- Infix equivalent of equiv
    infixl 6 !==!
    (!==!) :: t a -> t a -> Bool
    t1 !==! t2 = Tensor.Multilinear.equiv t1 t2
    -- switch all indices type
    transpose :: t a -> t a
    -- switch only one index type
    transpose1 :: t a -> t a
    -- concatenation of two tensor with given index or by creating a new one
    concat ::  TIndex -> t a -> t a -> t a
    -- infix version of concat
    infixl 3 ++
    (++) :: TIndex -> t a -> t a -> t a
    (++) = Tensor.Multilinear.concat




