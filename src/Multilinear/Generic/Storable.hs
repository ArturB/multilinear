{-|
Module      : Multilinear.Generic.GPU
Description : Generic implementation of tensor as nested arrays, evaluated on GPU using OpenCL
Copyright   : (c) Artur M. Brodzki, 2018
License     : BSD3
Maintainer  : artur@brodzki.org
Stability   : experimental
Portability : Windows/POSIX

-}

module Multilinear.Generic.GPU (
    -- * Generic tensor datatype and its instances
    Tensor(..), 
    -- * Auxiliary functions
    toPtrTensor, fromPtrTensor
) where

import           Control.DeepSeq
import qualified Control.Parallel.Strategies   as Parallel
import           Data.Foldable
import           Data.List
import           Data.Maybe
import qualified Data.Vector                   as Boxed
import qualified Data.Vector.Storable          as StorableV
import           Foreign.ForeignPtr
import           Foreign.Ptr
import           Foreign.ForeignPtr.Unsafe
import           Foreign.Storable
import           GHC.Generics
import           Multilinear.Class             as Multilinear
import qualified Multilinear.Index             as Index
import qualified Multilinear.Index.Finite      as Finite
import qualified Multilinear.Generic.PtrTensor as Ptr
import           System.IO.Unsafe

{-| Tensor defined recursively as scalar or list of other tensors -}
{-| @c@ is type of a container, @i@ is type of index size and @a@ is type of tensor elements -}
data Tensor a where
    {-| Scalar -}
    Scalar :: {
        scalarVal :: a
    } -> Tensor a
    {-| Simple, one-dimensional finite tensor -}
    SimpleFinite :: {
        tensorFiniteIndex :: Finite.Index,
        tensorScalars     :: StorableV.Vector a
    } -> Tensor a
    {-| Finite array of other tensors -}
    FiniteTensor :: {
        {-| Finite index "Mutltilinear.Index.Finite" of tensor -}
        tensorFiniteIndex :: Finite.Index,
        {-| Array of tensors on deeper recursion level -}
        tensorsFinite     :: Boxed.Vector (Tensor a)
    } -> Tensor a
    deriving (Eq, Generic)

toPtrTensor :: Storable a => Tensor a -> Ptr.Tensor a
toPtrTensor (Scalar x) = Ptr.Scalar x
toPtrTensor (SimpleFinite i ts) = let
    (ptr,_) = StorableV.unsafeToForeignPtr0 ts
    in Ptr.SimpleFinite i (unsafeForeignPtrToPtr ptr, StorableV.length ts)
toPtrTensor (FiniteTensor i ts) = Ptr.FiniteTensor i (toPtrTensor <$> ts)

fromPtrTensor :: Storable a => Ptr.Tensor a -> Tensor a
fromPtrTensor (Ptr.Scalar x) = Scalar x
fromPtrTensor (Ptr.SimpleFinite i (ptr,len)) = let
    fptr = unsafePerformIO $ newForeignPtr_ ptr
    ts = StorableV.unsafeFromForeignPtr0 fptr len
    in SimpleFinite i ts
fromPtrTensor (Ptr.FiniteTensor i ts) = FiniteTensor i (fromPtrTensor <$> ts)
