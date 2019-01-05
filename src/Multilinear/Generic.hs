{-|
Module      : Multilinear.Generic
Description : Re-export default tensor implementation and provides simple tensor converters
Copyright   : (c) Artur M. Brodzki, 2018
License     : BSD3
Maintainer  : artur@brodzki.org
Stability   : experimental
Portability : Windows/POSIX

-}

module Multilinear.Generic (
    module Multilinear.Generic.Sequential,
    sequentialToMultiCore,
    multiCoreToSequential,
) where

import           Data.Vector.Storable           as StorableV
import           Data.Vector                    as Boxed
import           Data.Vector.Unboxed            as Unboxed
import           Multilinear.Generic.Sequential
import qualified Multilinear.Generic.Sequential as Sequential
import qualified Multilinear.Generic.MultiCore  as MultiCore

-- | Convert Sequential tensor to MultiCore
sequentialToMultiCore :: (Unboxed.Unbox a, Storable a) => Sequential.Tensor a -> MultiCore.Tensor a
sequentialToMultiCore (Sequential.Scalar x) = MultiCore.Scalar x
sequentialToMultiCore (Sequential.SimpleFinite i ts) = MultiCore.SimpleFinite i ts
sequentialToMultiCore (Sequential.FiniteTensor i ts) = MultiCore.FiniteTensor i $ Boxed.map sequentialToMultiCore ts

-- | Convert MultiCore tensor to Sequential
multiCoreToSequential :: (Unboxed.Unbox a, Storable a) => MultiCore.Tensor a -> Sequential.Tensor a
multiCoreToSequential (MultiCore.Scalar x) = Sequential.Scalar x
multiCoreToSequential (MultiCore.SimpleFinite i ts) = Sequential.SimpleFinite i ts
multiCoreToSequential (MultiCore.FiniteTensor i ts) = Sequential.FiniteTensor i $ Boxed.map multiCoreToSequential ts
