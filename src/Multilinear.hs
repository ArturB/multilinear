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

module Multilinear (
    module Multilinear.Class,
    module Multilinear.Generic
) where

-- Re-export basic library modules
import           Multilinear.Class
import           Multilinear.Generic
