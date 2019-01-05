{-|
Module      : Multilinear.Generic.Storable
Description : Generic implementation of tensor as nested Storable vectors
Copyright   : (c) Artur M. Brodzki, 2018
License     : BSD3
Maintainer  : artur@brodzki.org
Stability   : experimental
Portability : Windows/POSIX

-}

module Multilinear.Generic.Storable (
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

