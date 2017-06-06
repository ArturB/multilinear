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

module Multilinear.Library (

) where

-- Re-export other library modules
import qualified Multilinear
import qualified Multilinear.Form                       as Form
import qualified Multilinear.Index                      as Index
import qualified Multilinear.Index.Finite               as Index.Finite
import qualified Multilinear.Index.Infinite             as Index.Infinite
import           Multilinear.Generic
import qualified Multilinear.Matrix                     as Matrix
import qualified Multilinear.NForm                      as NForm
import qualified Multilinear.NVector                    as NVector
import qualified Multilinear.Tensor                     as Tensor
import qualified Multilinear.Vector                     as Vector



import           Statistics.Distribution
import           Statistics.Distribution.Beta
import           Statistics.Distribution.Binomial
import           Statistics.Distribution.CauchyLorentz
import           Statistics.Distribution.ChiSquared
import           Statistics.Distribution.Exponential
import           Statistics.Distribution.FDistribution
import           Statistics.Distribution.Gamma
import           Statistics.Distribution.Geometric
import           Statistics.Distribution.Hypergeometric
import           Statistics.Distribution.Laplace
import           Statistics.Distribution.Normal
import           Statistics.Distribution.StudentT
import           Statistics.Distribution.Uniform
