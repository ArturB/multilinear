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
    sequentialToMultiCore, sequentialToGPU,
    multiCoreToSequential, multiCoreToGPU,
    gpuToSequential, gpuToMultiCore
) where

import           Data.Vector.Storable           as StorableV
import           Data.Vector                    as Boxed
import           Data.Vector.Unboxed            as Unboxed
import           Multilinear.Generic.Sequential
import qualified Multilinear.Generic.Sequential as Sequential
import qualified Multilinear.Generic.MultiCore  as MultiCore
import qualified Multilinear.Generic.GPU        as GPU

-- | Convert Sequential tensor to MultiCore
sequentialToMultiCore :: (Unboxed.Unbox a, Storable a) => Sequential.Tensor a -> MultiCore.Tensor a
sequentialToMultiCore (Sequential.Scalar x) = MultiCore.Scalar x
sequentialToMultiCore (Sequential.SimpleFinite i ts) = MultiCore.SimpleFinite i ts
sequentialToMultiCore (Sequential.FiniteTensor i ts) = MultiCore.FiniteTensor i $ Boxed.map sequentialToMultiCore ts

-- | Convert Sequential tensor to GPU
sequentialToGPU :: (Unboxed.Unbox a, Storable a) => Sequential.Tensor a -> GPU.Tensor a
sequentialToGPU (Sequential.Scalar x) = GPU.Scalar x
sequentialToGPU (Sequential.SimpleFinite i ts) = GPU.SimpleFinite i $ StorableV.generate (Unboxed.length ts) $ \i -> ts Unboxed.! i
sequentialToGPU (Sequential.FiniteTensor i ts) = GPU.FiniteTensor i $ Boxed.map sequentialToGPU ts

-- | Convert MultiCore tensor to Sequential
multiCoreToSequential :: (Unboxed.Unbox a, Storable a) => MultiCore.Tensor a -> Sequential.Tensor a
multiCoreToSequential (MultiCore.Scalar x) = Sequential.Scalar x
multiCoreToSequential (MultiCore.SimpleFinite i ts) = Sequential.SimpleFinite i ts
multiCoreToSequential (MultiCore.FiniteTensor i ts) = Sequential.FiniteTensor i $ Boxed.map multiCoreToSequential ts

-- | Convert MultiCore tensor to GPU
multiCoreToGPU :: (Unboxed.Unbox a, Storable a) => MultiCore.Tensor a -> GPU.Tensor a
multiCoreToGPU (MultiCore.Scalar x) = GPU.Scalar x
multiCoreToGPU (MultiCore.SimpleFinite i ts) = GPU.SimpleFinite i $ StorableV.generate (Unboxed.length ts) $ \i -> ts Unboxed.! i
multiCoreToGPU (MultiCore.FiniteTensor i ts) = GPU.FiniteTensor i $ Boxed.map multiCoreToGPU ts

-- | Convert GPU tensor to Sequential
gpuToSequential :: (Unboxed.Unbox a, Storable a) => GPU.Tensor a -> Sequential.Tensor a
gpuToSequential (GPU.Scalar x) = Sequential.Scalar x
gpuToSequential (GPU.SimpleFinite i ts) = Sequential.SimpleFinite i $ Unboxed.generate (StorableV.length ts) $ \i -> ts StorableV.! i
gpuToSequential (GPU.FiniteTensor i ts) = Sequential.FiniteTensor i $ Boxed.map gpuToSequential ts

-- | Convert GPU tensor to MultiCore
gpuToMultiCore :: (Unboxed.Unbox a, Storable a) => GPU.Tensor a -> MultiCore.Tensor a
gpuToMultiCore (GPU.Scalar x) = MultiCore.Scalar x
gpuToMultiCore (GPU.SimpleFinite i ts) = MultiCore.SimpleFinite i $ Unboxed.generate (StorableV.length ts) $ \i -> ts StorableV.! i
gpuToMultiCore (GPU.FiniteTensor i ts) = MultiCore.FiniteTensor i $ Boxed.map gpuToMultiCore ts
