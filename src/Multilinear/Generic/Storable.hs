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
) where

import qualified Data.Vector                   as Boxed
import qualified Data.Vector.Storable          as StorableV
import           GHC.Generics
import           Multilinear.Class
import qualified Multilinear.Index.Finite      as Finite

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

-- | Print tensor
instance (
    Multilinear Tensor a, Show a
    ) => Show (Tensor a) where

    -- merge errors first and then print whole tensor
    show = show' . standardize
        where
        show' x = case x of
            -- Scalar is showed simply as its value
            Scalar v -> show v
            -- SimpleFinite is shown dependent on its index...
            SimpleFinite index ts -> show index ++ "S: " ++ case index of
                -- If index is contravariant, show tensor components vertically
                Finite.Contravariant _ _ -> "\n" ++ tail (StorableV.foldl' (\string e -> string ++ "\n  |" ++ show e) "" ts)
                -- If index is covariant or indifferent, show tensor compoments horizontally
                _                        -> "["  ++ tail (StorableV.foldl' (\string e -> string ++ "," ++ show e) "" ts) ++ "]"
            -- FiniteTensor is shown dependent on its index...
            FiniteTensor index ts -> show index ++ "T: " ++ case index of
                -- If index is contravariant, show tensor components vertically
                Finite.Contravariant _ _ -> "\n" ++ tail (Boxed.foldl' (\string e -> string ++ "\n  |" ++ show e) "" ts)
                -- If index is covariant or indifferent, show tensor compoments horizontally
                _                        -> "["  ++ tail (Boxed.foldl' (\string e -> string ++ "," ++ show e) "" ts) ++ "]"
