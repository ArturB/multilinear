{-|
Module      : Multilinear.Tensor.AsArray
Description : Tensors in array ("Data.Vector") implementation
Copyright   : (c) Artur M. Brodzki, 2017
License     : GPL-3
Maintainer  : artur.brodzki@gmail.com
Stability   : experimental
Portability : Windows/POSIX

- This module provides convenient constructors that generate a arbitrary tensors in array ("Data.Vector") implementation. 
- Choice of container type has great impact on library performance in particular use cases
- Array ("Data.Vector") implementation is generally faster, however it is strict and always keeps all tensor elements in memory, so it may require large amount of RAM.
- List implementation is slower but lazy and when tensor is generated from indices or randomly, it does not generate all elements at once if not necessary,
so it may operate in smaller memory (e.g. linear instead of quadratic when multiplying matrix by vector or form).

-}

{-# LANGUAGE GADTs  #-}
{-# LANGUAGE Strict #-}

module Multilinear.Tensor.AsArray (
  
) where
