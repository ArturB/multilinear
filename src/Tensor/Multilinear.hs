-----------------------------------------------------------------------------
--
-- Package     :  Tensor
-- Module      :  Tensor
-- Description :  Defines main operations that every Tensor instance should provide
-- Author      :  Artur M. Brodzki, Warsaw 2016
-----------------------------------------------------------------------------

{-# LANGUAGE Strict, FlexibleInstances, FlexibleContexts, GADTs #-}
{-# OPTIONS_GHC -O2 #-}

module Tensor.Multilinear (
    Multilinear(..),
    Tensor(..), T(..), T_(..), V(..), el, ind
) where

import Tensor.Index

{- TENSOR CLASS - TENSOR DEFINED AS MULTILINEAR MAP -}

class Multilinear t where 
    -- Tensor sum
    infixl 7 !+
    (!+) :: t -> t -> t

    -- Tensor difference
    infixl 7 !-
    (!-) :: t -> t -> t

    -- Tensor product with Einstein summation convention
    infixl 8 !*
    (!*) :: t -> t -> t

class Num t => Tensor t where
    -- Projection
    --(!!) :: t a -> [(String,Int)] -> Maybe (t a)
    -- Generate tensor elements as a function of indices
    generate :: TIndex -> (Int -> t) -> t
    -- generate rank tensor from a list
    fromList :: TIndex -> [a] -> t
    -- Tensor order (contravariant, covariant)
    order :: t -> (Int,Int)
    -- Number of tensor elements
    elems :: t -> Int
    -- List of tensor indices
    indices :: t -> [TIndex]
    -- Rename tensor index
    rename :: t -> String -> String -> t
    -- Check if tensors are equivalent (are of the same type and size)
    equiv :: t -> t -> Bool
    -- Infix equivalent of equiv
    infixl 6 !==!
    (!==!) :: t -> t -> Bool
    t1 !==! t2 = Tensor.Multilinear.equiv t1 t2
    -- switch all indices type
    transpose :: t -> t
    -- switch only one index type
    transpose1 :: t -> t
    -- concatenation of two tensor with given index or by creating a new one
    concat ::  TIndex -> t -> t -> t
    -- infix version of concat
    --infixl 3 ++
    --(++) :: TIndex -> t -> t -> t
    --(++) = Tensor.Multilinear.concat


{-}
instance Num b => Num (a -> b) where
    (f1 + f2) x = f1 x + f2 x
    (f1 * f2) x = f1 x * f2 x
    (abs f) x = abs (f x)
    (signum f) x = signum (f x)
    (negate f) x = negate (f x)
-}


data T a where
    T :: String -> String -> ([Int] -> [Int] -> a) -> T a

data T_ a where
    T_ :: String -> ([Int] -> a) -> T_ a

data V a where
    V :: String -> ([Int] -> a) -> V a

el :: T a -> ([Int] -> [Int] -> a)
el (T _ _ f) = f

instance Num a => Num (T a) where
    (T us1 ds1 f1) + (T us2 ds2 f2) 
        | us1 == us2 && ds1 == ds2 = 
            T us1 ds1 (\ul dl -> f1 ul dl + f2 ul dl) 
        | otherwise = error "Tensors not compatible!"
    
    (T us1 ds1 f1) - (T us2 ds2 f2) 
        | us1 == us2 && ds1 == ds2 = T us1 ds1 (\ul dl -> f1 ul dl - f2 ul dl) 
        | otherwise = error "Tensors not compatible!"

    (T us1 ds1 f1) * (T us2 ds2 f2) = 
        T (us1 ++ us2) (ds1 ++ ds2) (
            \ul dl -> f1 (take (length us1) ul) (take (length ds1) dl) * f2 (drop (length us1) ul) (drop (length ds1) dl)
        )
    
    abs (T us ds f) = T us ds (\ul dl -> abs (f ul dl))

    signum (T us ds f) = T us ds (\ul dl -> signum (f ul dl))

    

ind :: T a -> (String,String)
ind (T us ds _) = (us,ds)