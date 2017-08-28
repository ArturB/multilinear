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

{-# OPTIONS_GHC -w #-}

module Library (
    module Form,
    module Generic,
    module Index,
    module Index.Finite,
    module Index.Infinite,
    module Matrix,
    module NForm,
    module Tensor,
    module Vector,
    module X,
    module Unsafe
) where

-- Re-export other library modules
import           Multilinear
import qualified Multilinear.Form                        as Form
import           Multilinear.Generic                     as Generic
import qualified Multilinear.Index                       as Index
import qualified Multilinear.Index.Finite                as Index.Finite
import qualified Multilinear.Index.Infinite              as Index.Infinite
import qualified Multilinear.Matrix                      as Matrix
import qualified Multilinear.NForm                       as NForm
import qualified Multilinear.NVector                     as NVector
import qualified Multilinear.Tensor                      as Tensor
import qualified Multilinear.Vector                      as Vector



import           Statistics.Distribution                 as X
import           Statistics.Distribution.Beta            as X
import           Statistics.Distribution.Binomial        as X
import           Statistics.Distribution.CauchyLorentz   as X
import           Statistics.Distribution.ChiSquared      as X
import           Statistics.Distribution.Exponential     as X
import           Statistics.Distribution.FDistribution   as X
import           Statistics.Distribution.Gamma           as X
import           Statistics.Distribution.Geometric       as X
import           Statistics.Distribution.Hypergeometric  as X
import           Statistics.Distribution.Laplace         as X
import           Statistics.Distribution.Normal          as X
import           Statistics.Distribution.StudentT        as X
import           Statistics.Distribution.Uniform         as X

import           System.IO.Unsafe                        as Unsafe
