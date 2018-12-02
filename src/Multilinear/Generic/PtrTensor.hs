module Multilinear.Generic.PtrTensor (
    PtrTensor(..), toPtrTensor, fromPtrTensor
) where

import qualified Data.Vector              as Boxed
import qualified Data.Vector.Storable     as StorableV
import           Foreign.ForeignPtr
import           Foreign.ForeignPtr.Unsafe
import           Foreign.Ptr
import           Foreign.Storable
import           GHC.Generics
import qualified Multilinear.Index.Finite as Finite
import qualified Multilinear.Generic.GPU  as GPU
import           System.IO.Unsafe

-- | Tensor with Unboxed.Vector replaced by C array
data PtrTensor a where
    {-| Scalar -}
    Scalar :: {
        scalarVal :: a
    } -> PtrTensor a
    {-| Simple, one-dimensional finite tensor -}
    SimpleFinite :: {
        tensorFiniteIndex :: Finite.Index,
        tensorScalars     :: (Ptr a, Int)
    } -> PtrTensor a
    {-| Finite array of other tensors -}
    FiniteTensor :: {
        {-| Finite index "Mutltilinear.Index.Finite" of tensor -}
        tensorFiniteIndex :: Finite.Index,
        {-| Array of tensors on deeper recursion level -}
        tensorsFinite     :: Boxed.Vector (PtrTensor a)
    } -> PtrTensor a
    deriving (Eq, Generic)

toPtrTensor :: Storable a => GPU.Tensor a -> PtrTensor a
toPtrTensor (GPU.Scalar x) = Scalar x
toPtrTensor (GPU.SimpleFinite i ts) = let
    (ptr,_) = StorableV.unsafeToForeignPtr0 ts
    in SimpleFinite i (unsafeForeignPtrToPtr ptr, StorableV.length ts)
toPtrTensor (GPU.FiniteTensor i ts) = FiniteTensor i (toPtrTensor <$> ts)

fromPtrTensor :: Storable a => PtrTensor a -> GPU.Tensor a
fromPtrTensor (Scalar x) = GPU.Scalar x
fromPtrTensor (SimpleFinite i (ptr,len)) = let
    fptr = unsafePerformIO $ newForeignPtr_ ptr
    ts = StorableV.unsafeFromForeignPtr0 fptr len
    in GPU.SimpleFinite i ts
fromPtrTensor (FiniteTensor i ts) = GPU.FiniteTensor i (fromPtrTensor <$> ts)
