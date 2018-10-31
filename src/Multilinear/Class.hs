{-|
Module      : Multilinear
Description : A (multi)linear algbra library.
Copyright   : (c) Artur M. Brodzki, 2018
License     : BSD3
Maintainer  : artur@brodzki.org
Stability   : experimental
Portability : Windows/POSIX

Multilinear library provides efficient and terse way to deal with linear algebra in Haskell. It is based on concept of tensor - a multidimensional indexed array of numbers. Vectors, matrices and linear functionals are examples of low-order tensors.

= Quick tutorial

Tensors are indexed, each having one or several indices numbering its components. A scalar has no indices, as being simply a value. A vector has one index, because it is a one-dimensional list of numbers. A matrix has two indices, being a bidimensional table of numbers. Tensors are arbitrarily - dimensional, so you can have a vector of matrices (third-order tensor) and so on. Such tensors are especially useful when dealing with more complex task, such as programming neural networks e.g. for deep learning.

Index may be either __lower (contravariant)__ or __upper (covariant)__, depending on its function.

A vector has one __upper__ or __contravariant__ index, because it represents a value, point in a space. A linear functional (called in this library as a __form__) is represented as a list of numbers, similar to vector, but it does not indicate a value, but rather a coefficients of __linear map__  or __transformation__ that takes a vector and returns a number, simply by calculating a linear combination of vector components with weights being a linear functional coefficients. So elements of linear functional are indexed by __lower__ or __covariant__ index.

A matrix also represents a linear transformation, similar to linear functional, but it take a vector and return a vector, not a scalar. So matrix is simply a bunch of linear functionals - each one is a matrix row and returns a one element of resulting vector. These linear functionals are also indexed - of course by an contravariant index, as vector, which is returned by a matrix. So matrix is a tensor with two indices - one uppper (contravariant) and one lower (covariant).

A dot product is - what a surprise! - a linear transformation. But it is different from matrix - it takes two vectors to multiplicate and returns one number. So it needs two lower (covariant) indices. In fact, a dot product is represented as a bidimensional table of numbers - as matrix - but with two lower indices, instead of one upper and one lower. Tensors with several lower indices are called n-forms. Actually, many linear maps that takes two vectors and returns a number are represented in such notation. A dot product is a table filled with zeros but having a 1-s in diagonal of the table - it look visually identical like unit matrix, but it isn't a matrix - it's actually a 2-form. In this library dot product is called (as in abstract maths) a __Kronecker delta__ (in "Multilinear.NForm"").

A cross product takes two vectors and returns a vector of numbers. So, as you can guess, it has three indices - two covariant (lower) and one upper (contravariant). A tensor that corresponds to cross product is called Levi-Civita symbol and is implemented under "Multilinear.Tensor".

As you see, multidimensional indexed arrays - tensors - provide a unified way to deal with linear algebra - such with data (vectors) and functions operating on them (matrices, n-forms). Using tensors instead of limited number of pre-defined functions, you gain a power of universal formalism, where you can express any linear function you want to deal with.

When you apply a linear functional to a vector, you compute a linear combination - or a weighted sum - of its components. It is so common operation in linear algebra,
that it requires a convenient way to note. Einstein (1905) introduced a summation rule:

If you multiply a tensor with lower index __/i/__ by a tensor with upper index with same name __/i__ then the linear combination of its components is automatically computed. The only condition is that an index of vector and index of corresponding linear transformation must have the same name.

== Examples

@

>>> v = Vector.fromIndices "i" 5 $ \\i -> i + 2 (vector v has 5 elements and v[i] = i + 2, indexing from 0)
>>> v
\<i:5\>
   | 2
   | 3
   | 4
   | 5
   | 6
>>> f = Form.const "i" 5 1 (linear functional has 5 elements and f[i] = 1)
>>> f
[i:5] [1,1,1,1,1]
>>> f * v
20
@

If you want to apply a vector to matrix, you must simply multiply them. The only condition is - as you can guess - that lower (indicating that matrix is a linear transformation) index of matrix must have the same name as the upper index of vector (indicating that this vector is a argument of matrix linear transformation; an upper index of matrix here indicates that this linear transformation returns a vector)

@

>>> m = Matrix.fromIndices "ij" 5 5 $ \\i j -> i + j
>>> m
\<i:5\>
   | [j:5] [0,1,2,3,4]
   | [j:5] [1,2,3,4,5]
   | [j:5] [2,3,4,5,6]
   | [j:5] [3,4,5,6,7]
   | [j:5] [4,5,6,7,8]
>>> v = Vector.fromIndices "j" 5 $ \\j -> j (vector v has 5 elements and v[j] = j, indexing from 0)
>>> v
\<j:5\>
   | 0
   | 1
   | 2
   | 3
   | 4
>>> m * v
\<i:5\>
   | 30
   | 40
   | 50
   | 60
   | 70
@
Note, that result vector is indexed with "i" - the same index as uppper index of our matrix.

If you want to do a matrix multiplication, a lower index of first matrix must have the same name as upper index of second matrix.

@

>>> m1 = Matrix.fromIndices "ij" 3 5 $ \\i j -> i + j
>>> m1
\<i:3\>
   | [j:5] [0,1,2,3,4]
   | [j:5] [1,2,3,4,5]
   | [j:5] [2,3,4,5,6]
>>> m2 = Matrix.fromIndices "jk" 5 4 $ \\j -> i + j (vector v has 5 elements and v[j] = j, indexing from 0)
>>> m2
\<j:5\>
   | [k:4] [0,1,2,3]
   | [k:4] [1,2,3,4]
   | [k:4] [2,3,4,5]
   | [k:4] [3,4,5,6]
   | [k:4] [4,5,6,7]
>>> m1 * m2

\<i:3\>
   | [k:4] [30,40,50,60]
   | [k:4] [40,50,60,70]
   | [k:4] [50,60,70,80]
@

Note, that rule that first matrix lower index must be the same as the second matrix upper index corresponeds to the fact, that to multiply two matrices, the widht of first matrix must be the same as width of the second matrix. The rule of matrix multiplication guarantees, that this operation is equivalent to linear functions composition.

The dot product of vectors may be done by simply making one of vectors with lower index, or - more correctly - by applying two vectors to Kronecker delta:

@

>>> v1 = Vector.fromIndices "i" 5 $ \\i -> i + 2 (vector v has 5 elements and v[i] = i + 2, indexing from 0)
>>> v1
\<i:5\>
   | 2
   | 3
   | 4
   | 5
   | 6
>>> v2 = Vector.fromIndices "j" 5 $ \\j -> j (vector v has 5 elements and v[j] = j, indexing from 0)
>>> v2
\<j:5\>
   | 0
   | 1
   | 2
   | 3
   | 4
>>> v1 * (lower "j" v2)
50
>>> NForm.dot 5 "ij" * v1 * v2 (A Kronecker delta - representing a dot product - of size 5 with indices \"i\" and \"j\" is multiplied by v1 and v2)
50

@

If you want to know more about linear algebra and Einstein convention, read Wikipedia:

- <https://en.wikipedia.org/wiki/Matrix_(mathematics)>
- <https://en.wikipedia.org/wiki/Covariance_and_contravariance_of_vectors>
- <https://en.wikipedia.org/wiki/Einstein_notation>

-}

module Multilinear.Class (
    Multilinear(..),
    Accessible(..)
) where

import           Data.Maybe
import           Data.Set
import           Multilinear.Index

{-| Multidimensional array treated as multilinear map - tensor -}
class (
  Num (t a),     -- Tensors may be added, subtracted and multiplicated
  Monoid (t a),  -- Tensors are monoids with concatenation as monoid operation
  Functor t      -- Tensor should be a Functor for convenience
  ) => Multilinear t a where

    {-| Add scalar @a@ to each element of tensor @t@ -}
    infixl 7 +.
    (+.) :: a -> t a -> t a

    {-| Subtract each element of tensor @t@ from scalar scalar left -}
    infixl 7 -.
    (-.) :: a -> t a -> t a

    {-| Multiply scalar @a@ by each element of tensor @t@ -}
    infixl 8 *.
    (*.) :: a -> t a -> t a

    {-| Add each element of tensor @t@ to scalar @a@ -}
    infixl 7 .+
    (.+) :: t a -> a -> t a

    {-| Subtract scalar @a@ from each element of tensor @t@ -}
    infixl 7 .-
    (.-) :: t a -> a -> t a

    {-| Multiply each element of tensor @t@ by scalar @a@ -}
    infixl 8 .*
    (.*) :: t a -> a -> t a

    {-| Tensor adding - functionally equal to Num (+) but more efficient -}
    infixl 7 .+.
    (.+.) :: t a -> t a -> t a

    {-| Tensor subtracting - functionally equal to Num (-) but more efficient -}
    infixl 7 .-.
    (.-.) :: t a -> t a -> t a

    {-| Tensor multiplication - functionally equal to Num (*) but more efficient -}
    infixl 7 .*.
    (.*.) :: t a -> t a -> t a

    {-| List of all tensor indices -}
    indices :: t a -> [TIndex]

    {-| List of tensor indices names -}
    indicesNames :: t a -> [String]
    indicesNames t = indexName <$> indices t

    {-| Tensor order - number of covariant and contravariant indices -}
    {-| @order t = (cv, cov)@ where @cv@ is number of upper and @cov@ is number of lower indices -}
    order :: t a -> (Int,Int)

    {-| Return size of index with given name -}
    size :: t a -> String -> Int

    {-| Check if tensors are equivalent (have same indices but in different order) -}
    equiv :: t a -> t a -> Bool
    equiv t1 t2 = Data.Set.fromList (indices t1) == Data.Set.fromList (indices t2)

    {-| Infix equivalent of 'equiv'. Has low priority equal to 1. |-}
    infixl 1 |==|
    (|==|) :: t a -> t a -> Bool
    t1 |==| t2 = equiv t1 t2

    {-| @t $| "ij" "kl"@ renames upper indices of tensor @t@ to @ij@ and lower indices to @kl@ -}
    infix 8 $|
    ($|) :: t a -> (String,String) -> t a

    {-| @raise t "i"@ raises an index @i@ of tensor @t@ -}
    raise :: t a -> String -> t a
    raise t i = t /\ i

    {-| Infix equivalent of 'raise' -}
    infixl 7 /\
    (/\) :: t a -> String -> t a
    t /\ i = raise t i

    {-| @lower t "i"@ lowers an index @i@ of tensor @t@ -}
    lower :: t a -> String -> t a
    lower t i = t \/ i

    {-| Infix equivalent of 'lower' -}
    infixl 7 \/
    (\/) :: t a -> String -> t a
    t \/ i = lower t i

    {-| Switch all indices of tensor @t@ - upper indices becomes lower and vice versa -}
    transpose :: t a -> t a

    {-| Shift tensor index right -}
    {-| @shiftRight t "i"@ moves index @i@ of tensor @t@ one level depeer in recursion.
        Elements of tensor as indexed with indices names becomes unchanged. -}
    shiftRight :: t a -> String -> t a
    -- ^ Right shift of an index is equivalent to left shift of its successor in recursion @s@, if only @s@ exists, so:
    -- Given a tensor @t[i1,i2,i3,...]@: @shiftRight t "i2" == t[i1,i3,i2,...] == shiftLeft t "i3"@
    shiftRight t n
        | isJust $ successor n (indicesNames t) = shiftLeft t (fromJust $ successor n (indicesNames t))
        | otherwise = t
        where
        successor x (a1:a2:as) = if x == a1 then Just a2 else successor x (a2:as)
        successor _ _ = Nothing

    {-| Infix equivalent of 'shiftRight' -}
    {-| @t |>> "i"@ moves index @i@ of tensor @t@ one level depeer in recursion -}
    infixl 9 |>>
    (|>>) :: t a -> String -> t a
    t |>> n = shiftRight t n

    {-| Shift tensor index rightmost -}
    {-| @shiftRightmost t "i"@ moves index @i@ of tensor @t@ to the deepest level in recursion.
        Elements of tensor as indexed with indices names becomes unchanged.  -}
    shiftRightmost :: t a -> String -> t a
    shiftRightmost t n = until (\x -> n == last (indicesNames x)) (|>> n) t

    {-| Infix equivalent of 'shiftRightmost' -}
    {-| @t |>>> "i"@ moves index @i@ of tensor @t@ to the deepest level in recursion -}
    infixl 9 |>>>
    (|>>>) :: t a -> String -> t a
    t |>>> n = shiftRightmost t n

    {-| Shift tensor index left. Elements of tensor as indexed with indices names becomes unchanged. -}
    {-| @shiftLeft t "i"@ moves index @i@ of tensor @t@ one level up in recursion -}
    shiftLeft :: t a -> String -> t a
    -- ^ Left shift of an index is equivalent to right shift of its predecessor in recursion @p@, if only @p@ exists, so:
    -- Given a tensor t[i1,i2,i3,...]: @shiftLeft t "i3" == t[i1,i3,i2,...] == shiftRight t "i2"@
    shiftLeft t n
        | isJust (predecessor n (indicesNames t)) = shiftRight t (fromJust $ predecessor n (indicesNames t))
        | otherwise = t
        where
        predecessor x (a1:a2:as) = if x == a2 then Just a1 else predecessor x (a2:as)
        predecessor _ _ = Nothing

    {-| Infix equivalent to 'shiftLeft' -}
    {-| @t <<| "i"@ moves index @i@ of tensor @t@ one level up in recursion -}
    infixl 9 <<|
    (<<|) :: t a -> String -> t a
    t <<| n = shiftLeft t n

    {-| Shift tensor index leftmost. Elements of tensor as indexed with indices names becomes unchanged. -}
    {-| @shiftLeftmost t "i"@ moves index @i@ of tensor @t@ to the first level in recursion -}
    shiftLeftmost :: t a -> String -> t a
    shiftLeftmost t n = until (\x -> n == head (indicesNames x)) (<<| n) t

    {-| Infix equivalent of 'shiftLeftmost' -}
    {-| @t <<<| "i"@ moves index @i@ of tensor @t@ to the first level in recursion -}
    infixl 9 <<<|
    (<<<|) :: t a -> String -> t a
    t <<<| n = shiftLeftmost t n

    {-| Concatenation of two tensors by common index -}
    {-| Tensors must be equivalent: 'equiv' t1 t2 == True -}
    augment ::  t a -> t a -> String -> t a

    {-| Simple mapping -}
    {-| @map f t@ returns tensor @t2@ in which @t2[i1,i2,...] = f t[i1,i2,...]@ -}
    map :: (a -> b) -> t a -> t b
    map = fmap


{-| If container on which tensor instance is built, allows for random access of its elements, then the tensor can be instanced as Accessible -}
class Multilinear t a => Accessible t a where

    {-| Accessing tensor elements -}
    {-| @el ["i","j"] t [4,5]@ returns all tensor elements which index @i@ is equal to 4 and index @j@ is equal to 5.
        Values of other indices are insignificant -}
    {-| If given index value is out of range, then modulo operation is performed:
        el ["i","j"] t [40 50] = t[40 mod size i, 50 mod size j] -}
    el :: t a -> (String,[Int]) -> t a

    {-| Infix equivalent for el -}
    infixl 9 $$|
    ($$|) :: t a -> (String,[Int]) -> t a
    t $$| is = el t is

    {-| Mapping with indices - mapping function takes not only a tensor element value but also its indices in tensor -}
    {-| @iMap f t@ return tensor @t2@ in which @t2[i1,i2,...] = f [i1,i2,...] t[i1,i2,...]@ -}
    iMap :: ([Int] -> a -> b) -> t a -> t b
    -- // TODO
