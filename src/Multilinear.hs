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

{-# LANGUAGE GADTs  #-}
{-# LANGUAGE Strict #-}
{-# OPTIONS_GHC -O2 #-}

module Multilinear (
    --module X
) where

-- Re-export other library modules
import           Multilinear.Covector       as X
import           Multilinear.Covector.Field as X
import           Multilinear.Covector.Seq   as X
import           Multilinear.Index          as X
import           Multilinear.ListTensor
import           Multilinear.Matrix         as X
import           Multilinear.Matrix.Field   as X
import           Multilinear.Matrix.Seq     as X
import           Multilinear.NForm          as X
import           Multilinear.NForm.Field    as X
import           Multilinear.NForm.Seq      as X
import           Multilinear.NVector        as X
import           Multilinear.NVector.Field  as X
import           Multilinear.NVector.Seq    as X
import           Multilinear.Tensor         as X
import           Multilinear.Tensor.Field   as X
import           Multilinear.Tensor.Seq     as X
import           Multilinear.Vector         as X
import           Multilinear.Vector.Field   as X
import           Multilinear.Vector.Seq     as X
