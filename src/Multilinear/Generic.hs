{-|
Module      : Multilinear.Generic
Description : Type family for all tensor-like datatypes
Copyright   : (c) Artur M. Brodzki, 2017
License     : GPL-3
Maintainer  : artur.brodzki@gmail.com
Stability   : experimental
Portability : Windows/POSIX

Type family for all tensor-like types.

-}


{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
--{-# LANGUAGE Strict                     #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Multilinear.Generic (
    -- * Datatype family
    Tensor,
    -- * Type aliases
    ListTensor,
    VectorTensor,
    UnboxedVectorTensor,
    -- * "Control.Applicative"" instances
    ZipVector(..),
    ZipUnboxedVector(..)
) where

import           Control.Applicative
import           Control.Monad
import qualified Data.Vector         as Boxed
import qualified Data.Vector.Unboxed as Unboxed
import           GHC.Generics

{-| Family of all recursive tensor types, parametrized by container type and elements type -}
data family Tensor :: (* -> *) -> * -> *

{-| List implementation of tensor must be done with ZipList -}
{-| Common List Applicative instance is invalid -}
type ListTensor = Tensor ZipList

{-| ZipList must be a Monoid instance to work with it on tensors -}
deriving instance Monoid (ZipList a)

{-| List implementation of tensor must be done with ZipList -}
{-| Common List Applicative instance is invalid -}
type VectorTensor = Tensor ZipVector

{-| List implementation of tensor must be done with ZipList -}
{-| Common List Applicative instance is invalid -}
type UnboxedVectorTensor = Tensor ZipUnboxedVector

{-| Newtype wrapper for boxed Vector, providing proper Applicative instance -}
newtype ZipVector a =
    ZipVector {
        getZipVector :: Boxed.Vector a
     } deriving (
         Eq, Show, Read, Ord,
         Monad, MonadPlus, Alternative,
         Functor, Foldable,
         Monoid, Generic)

{-| ZipVector Applicative instance -}
instance Applicative ZipVector where
    pure = ZipVector . Boxed.singleton

    ZipVector v1 <*> ZipVector v2 = ZipVector $ (\(f,p) -> f p) <$> Boxed.zip v1 v2

{-| Newtype wrapper for unboxed Vector, provising Applicative instance -}
newtype ZipUnboxedVector a =
    ZipUnboxedVector {
        getZipUnboxedVector :: Unboxed.Vector a
     } deriving (
         Eq, Show, Read, Ord,
         Monoid, Generic)
