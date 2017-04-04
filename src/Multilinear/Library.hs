{-|
Module      : Multilinear.Library
Description : Provides efficient and generic implementation of linear algebra operation using Ricci - Einstein tensor formalism
Copyright   : (c) Artur M. Brodzki, 2017
License     : 3-clause BSD
Maintainer  : artur.brodzki@gmail.com
Stability   : experimental
Portability : Windows/POSIX

Defines main tyclasses to deal with multilinear algebra and re-exports content of other library modules.

-}

{-# LANGUAGE GADTs  #-}
{-# LANGUAGE Strict #-}
{-# OPTIONS_GHC -O2 #-}

module Multilinear.Library (
    --module X,
    --module Form,
    --module Matrix,
    --module NForm,
    --module NVector,
    --module Tensor,
    --module Vector
) where

-- Re-export other library modules
import qualified Multilinear
import qualified Multilinear.Form           as Form
import qualified Multilinear.Index          as Index
import qualified Multilinear.Matrix         as Matrix
import qualified Multilinear.NForm          as NForm
import qualified Multilinear.NVector        as NVector
import qualified Multilinear.Tensor         as Tensor
import qualified Multilinear.Vector         as Vector
