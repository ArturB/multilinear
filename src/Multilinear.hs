{-|
Module      : Multilinear
Description : Provides efficient and generic implementation of linear algebra operation using Ricci - Einstein tensor formalism
Copyright   : (c) Artur M. Brodzki, 2017
License     : GPL-3
Maintainer  : artur.brodzki@gmail.com
Stability   : experimental
Portability : Windows/POSIX



Examples:

@
Prelude>Matrix.fromIndices "ij" 10 10 $ \i j -> i + j
line 2 
line 3
@

prop> a .+ t = t +. a

-}

{-# LANGUAGE GADTs  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict #-}
{-# OPTIONS_GHC -O2 #-}

module Multilinear (
    Multilinear(..), 
    module Multilinear.Index
) where

import Data.Maybe
import Data.Set
import Multilinear.Index
import Data.Bits

{-| Multidimensional array treated as multilinear map - tensor -}
class (
  Num (t i a),        -- Tensors may be added, subtracted and multiplicated
  Integral i,         -- Indices of tensors must be of integral type that allow to sum them finitely
  Bits (t i a),       -- Also bit operations can be performed on tensors
  Functor (t i)       -- Tensor is a multidimensional array and should be a Functor for convenience
  ) => Multilinear t i a where

    {-| Add scalar @a@ left to tensor @t@ producing tensor @a + t@ -}
    -- prop> a .+ t = t +. a
    infixl 7 .+
    (.+) :: a     -- ^ Scalar a
         -> t i a -- ^ Tensor t
         -> t i a -- ^ Tensor a + t

    {-| Subtract scalar left -}
    infixl 7 .-
    (.-) :: a       
         -> t i a 
         -> t i a

    {-| Multiply by scalar left-}
    infixl 8 .*
    (.*) :: a -> t i a -> t i a

    {-| Add scalar right -}
    infixl 7 +.
    (+.) :: t i a -> a -> t i a

    {-| Subtract scalar right -}
    infixl 7 -.
    (-.) :: t i a -> a -> t i a

    {-| Multiply by scalar right-}
    infixl 8 *.
    (*.) :: t i a -> a -> t i a

    -- | List of tensor indices \n
    -- @indices t@ return list of all indices of tensor @t@. 
    indices :: t i a       -- ^ tensor @t@
            -> [TIndex i]  -- ^ list of indices of tensor @t@
        
    {-| Number of tensor elements -}
    elements :: t i a -> i
    elements t = Prelude.foldl (*) 0 (Prelude.map indexSize $ indices t)

    {-| List of tensor indices names |-}
    indicesNames :: t i a -> [String]
    indicesNames = Prelude.map indexName . indices
    
    {-| Tensor order (contravariant, covariant) -}
    order :: t i a -> (Int,Int)

    {-| Check if tensors are equivalent (have same indices but in different order) -}
    equiv :: Ord a => t i a -> t i a -> Bool
    equiv t1 t2 = Data.Set.fromList (indices t1) == Data.Set.fromList (indices t2)

    {-| Infix equivalent of 'equiv' |-}
    (|==|) :: Ord a => t i a -> t i a -> Bool
    t1 |==| t2 = equiv t1 t2

    {-| Rename tensor index -}
    rename :: t i a -> String -> String -> t i a

    {-| Raise an index |-}
    raise :: t i a -> String -> t i a
    raise t i = t /\ i

    {-| Infix equivalent of raising index |-}
    (/\) :: t i a -> String -> t i a
    t /\ i = raise t i

    {-| Lower and index |-}
    lower :: t i a -> String -> t i a
    lower t i = t \/ i

    {-| Infix equivalent of lowering index |-}
    (\/) :: t i a -> String -> t i a
    t \/ i = lower t i

    {-| Switch all indices type -}
    transpose :: t i a -> t i a

    {-| Shift tensor index right |-}
    {-| Moves given index one level depeer in recursion |-}
    shiftRight :: t i a -> String -> t i a
    -- Left shift of an index is equivalent to right shift of its successor, if only it exists
    shiftRight t n
        | isJust $ successor n (indicesNames t) = shiftRight t (fromJust $ successor n (indicesNames t))
        | otherwise = t
        where
        successor x (a1:a2:as) = if x == a1 then Just a2 else successor x (a2:as)
        successor _ _ = Nothing

    {-| Infix equivalent to right shift |-}
    (|>>) :: t i a -> String -> t i a
    t |>> n = shiftRight t n

    {-| Shift tensor index rightmost |-}
    {-| Moves given index to the deepest level in recursion |-}
    shiftRightmost :: t i a -> String -> t i a
    shiftRightmost t n = until (\x -> n == last (indicesNames x)) (|>> n) t

    {-| Infix equivalent of shiftRightmost |-}
    (|>>>) :: t i a -> String -> t i a
    t |>>> n = shiftRightmost t n

    {-| Shift tensor index left |-}
    {-| Moves given index one level up in recursion |-}
    shiftLeft :: t i a -> String -> t i a
    -- Right shift of an index is equivalent to left shift of its predecessor, if only it exists
    shiftLeft t n
        | isJust $ predecessor n (indicesNames t) = shiftRight t (fromJust $ predecessor n (indicesNames t))
        | otherwise = t
        where
        predecessor x (a1:a2:as) = if x == a2 then Just a1 else predecessor x (a2:as)
        predecessor _ _ = Nothing

    {-| Infix equivalent to left shift |-}
    (<<|) :: t i a -> String -> t i a
    t <<| n = shiftRight t n

    {-| Shift tensor index leftmost |-}
    {-| Moves given index to the first level in recursion |-}
    shiftLeftmost :: t i a -> String -> t i a
    shiftLeftmost t n = until (\x -> n == last (indicesNames x)) (<<| n) t

    {-| Infix equivalent of shiftLeftmost |-}
    (<<<|) :: t i a -> String -> t i a
    t <<<| n = shiftLeftmost t n

    {-| Concatenation of two tensor by common index  -}
    augment ::  t i a -> t i a -> String -> t i a

    {-| Accessing tensor elements |-}
    el :: [String] -> t i a -> [i] -> t i a

    {-| Simple filtering |-}
    --filter :: (a -> Bool) -> t i a -> Maybe (t i a)

    {-| Filtering with indices |-}
    --iFilter :: [String] -> ([i] -> a -> Bool) -> t i a -> Maybe (t i a)

    {-| Simple mapping |-}
    map :: (a -> b) -> t i a -> t i b
    map = fmap

    {-| Mapping with indices |-}
    iMap :: ([i] -> a -> b) -> t i a -> t i b


{-| If tensor elements are fractional, then tensors may be fractional too |-}
--instance (Fractional a, Multilinear t i a) => Fractional (t i a) where
