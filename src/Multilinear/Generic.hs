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
    toMultiCore, toSequential
) where

import           Data.Vector                    as Boxed
import           Multilinear.Generic.Sequential
import qualified Multilinear.Generic.Sequential as Sequential
import qualified Multilinear.Generic.MultiCore  as MultiCore

-- | Convert Sequential tensor to MultiCore
toMultiCore :: Sequential.Tensor a -> MultiCore.Tensor a
toMultiCore (Sequential.Scalar x) = MultiCore.Scalar x
toMultiCore (Sequential.SimpleFinite i ts) = MultiCore.SimpleFinite i ts
toMultiCore (Sequential.FiniteTensor i ts) = MultiCore.FiniteTensor i $ Boxed.map toMultiCore ts

-- | Convert MultiCore tensor to Sequential
toSequential :: MultiCore.Tensor a -> Sequential.Tensor a
toSequential (MultiCore.Scalar x) = Sequential.Scalar x
toSequential (MultiCore.SimpleFinite i ts) = Sequential.SimpleFinite i ts
toSequential (MultiCore.FiniteTensor i ts) = Sequential.FiniteTensor i $ Boxed.map toSequential ts
