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

import           Data.List
import qualified Data.Vector                   as Boxed
import qualified Data.Vector.Storable          as StorableV
import           GHC.Generics
import           Multilinear.Class
import qualified Multilinear.Index            as Index
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


-- Multilinear operations
instance Multilinear Tensor Double where
    -- Generic tensor constructor
    -- If only one upper index is given, generate a SimpleFinite tensor with upper index
    fromIndices [u] [] [s] [] f = 
        SimpleFinite (Finite.Contravariant s [u]) $ StorableV.generate s $ \x -> f [x] []
  
    -- If only one lower index is given, generate a SimpleFinite tensor with lower index
    fromIndices [] [d] [] [s] f = 
        SimpleFinite (Finite.Covariant s [d]) $ StorableV.generate s $ \x -> f [] [x]
  
    -- If many indices are given, first generate upper indices recursively from indices list
    fromIndices (u:us) d (s:size) dsize f =
        FiniteTensor (Finite.Contravariant s [u]) $ Boxed.generate s (\x -> fromIndices us d size dsize (\uss dss -> f (x:uss) dss) )
  
    -- After upper indices, generate lower indices recursively from indices list
    fromIndices u (d:ds) usize (s:size) f =
        FiniteTensor (Finite.Covariant s [d]) $ Boxed.generate s (\x -> fromIndices u ds usize size (\uss dss -> f uss (x:dss)) )
  
    -- If there are indices without size or sizes without names, throw an error
    fromIndices _ _ _ _ _ = error "Indices and its sizes incompatible!"
  
    {-| Accessing tensor elements -}
    {-# INLINE el #-}
    -- Scalar has only one element
    el (Scalar x) _ = Scalar x
    -- simple tensor case
    el t1@(SimpleFinite index1 ts) (inds,vals) =
            -- zip indices with their given values
        let indvals = zip inds vals
            -- find value for simple tensor index if given
            val = Data.List.find (\(n,_) -> [n] == Index.indexName index1) indvals
            -- if value for current index is given
        in  if isJust val
            -- then get it from current tensor
            then Scalar $ ts StorableV.! snd (fromJust val)
            -- otherwise return whole tensor - no filtering defined
            else t1
    -- finite tensor case
    el (FiniteTensor index1 v1) (inds,vals) =
            -- zip indices with their given values
        let indvals = zip inds vals
            -- find value for current index if given
            val = Data.List.find (\(n,_) -> [n] == Index.indexName index1) indvals
            -- and remove used index from indices list
            indvals1 = Data.List.filter (\(n,_) -> [n] /= Index.indexName index1) indvals
            -- indices unused so far
            inds1 = Data.List.map fst indvals1
            -- and its corresponding values
            vals1 = Data.List.map snd indvals1
            -- if value for current index was given
        in  if isJust val
            -- then get it from current tensor and recursively process other indices
            then el (v1 Boxed.! snd (fromJust val)) (inds1,vals1)
            -- otherwise recursively access elements of all child tensors
            else FiniteTensor index1 $ (\t -> el t (inds,vals)) <$> v1

    -- List of all tensor indices
    {-# INLINE indices #-}
    indices x = case x of
        Scalar _            -> []
        FiniteTensor i ts   -> Index.toTIndex i : indices (head $ toList ts)
        SimpleFinite i _    -> [Index.toTIndex i]

    -- Get tensor order [ (contravariant,covariant)-type ]
    {-# INLINE order #-}
    order x = case x of
        Scalar _ -> (0,0)
        SimpleFinite index _ -> case index of
            Finite.Contravariant _ _ -> (1,0)
            Finite.Covariant _ _     -> (0,1)
        FiniteTensor _ ts -> let (cnvr, covr) = order $ (ts Boxed.! 0)
             in case (head $ indices x) of
                Index.Contravariant _ _ -> (cnvr+1,covr)
                Index.Covariant _ _     -> (cnvr,covr+1)

    -- Get size of tensor index or Left if index is infinite or tensor has no such index
    {-# INLINE size #-}
    size t iname = case t of
        Scalar _             -> error scalarIndices
        SimpleFinite index _ -> 
            if Index.indexName index == iname 
            then Finite.indexSize index 
            else error indexNotFound
        FiniteTensor index ts -> 
            if Index.indexName index == iname
            then Finite.indexSize index
            else size (ts Boxed.! 0) iname

    -- Rename tensor indices
    {-# INLINE ($|) #-}
    
    Scalar x $| _ = Scalar x
    SimpleFinite (Finite.Contravariant isize _) ts $| (u:_, _) = SimpleFinite (Finite.Contravariant isize [u]) ts
    SimpleFinite (Finite.Covariant isize _) ts $| (_, d:_) = SimpleFinite (Finite.Covariant isize [d]) ts
    FiniteTensor (Finite.Contravariant isize _) ts $| (u:us, ds) = FiniteTensor (Finite.Contravariant isize [u]) $ ($| (us,ds)) <$> ts
    FiniteTensor (Finite.Covariant isize _) ts $| (us, d:ds) = FiniteTensor (Finite.Covariant isize [d]) $ ($| (us,ds)) <$> ts
    t $| _ = t

    -- Raise an index
    {-# INLINE (/\) #-}
    Scalar x /\ _ = Scalar x
    FiniteTensor index ts /\ n
        | Index.indexName index == n =
            FiniteTensor (Finite.Contravariant (Finite.indexSize index) n) $ (/\ n) <$> ts
        | otherwise =
            FiniteTensor index $ (/\ n) <$> ts
    t1@(SimpleFinite index ts) /\ n
        | Index.indexName index == n =
            SimpleFinite (Finite.Contravariant (Finite.indexSize index) n) ts
        | otherwise = t1

    -- Lower an index
    {-# INLINE (\/) #-}
    Scalar x \/ _ = Scalar x
    FiniteTensor index ts \/ n
        | Index.indexName index == n =
            FiniteTensor (Finite.Covariant (Finite.indexSize index) n) $ (\/ n) <$> ts
        | otherwise =
            FiniteTensor index $ (\/ n) <$> ts
    t1@(SimpleFinite index ts) \/ n
        | Index.indexName index == n =
            SimpleFinite (Finite.Covariant (Finite.indexSize index) n) ts
        | otherwise = t1

    {-| Transpose a tensor (switch all indices types) -}
    {-# INLINE transpose #-}
    transpose (Scalar x) = Scalar x

    transpose (FiniteTensor (Finite.Covariant count name) ts) =
        FiniteTensor (Finite.Contravariant count name) (Multilinear.transpose <$> ts)
    transpose (FiniteTensor (Finite.Contravariant count name) ts) =
        FiniteTensor (Finite.Covariant count name) (Multilinear.transpose <$> ts)

    transpose (SimpleFinite (Finite.Covariant count name) ts) =
        SimpleFinite (Finite.Contravariant count name) ts
    transpose (SimpleFinite (Finite.Contravariant count name) ts) =
        SimpleFinite (Finite.Covariant count name) ts


    {-| Shift tensor index right -}
    {-| Moves given index one level deeper in recursion -}
    -- Scalar has no indices to shift
    Scalar x `shiftRight` _ = Scalar x
    -- Simple tensor has only one index which cannot be shifted
    t1@(SimpleFinite _ _) `shiftRight` _ = t1
    -- Finite tensor is shifted by converting to list and transposing it
    t1@(FiniteTensor index1 ts1) `shiftRight` ind
        -- We don't shift this index
        | Data.List.length (indicesNames t1) > 1 && Index.indexName index1 /= ind =
            FiniteTensor index1 $ (|>> ind) <$> ts1
        -- We found index to shift
        | Data.List.length (indicesNames t1) > 1 && Index.indexName index1 == ind =
                -- Next index
            let index2 = tensorFiniteIndex (ts1 Boxed.! 0)
                -- Elements to transpose
                dane = if isSimple (ts1 Boxed.! 0)
                       then (\un -> Boxed.generate (StorableV.length un) (\i -> Scalar $ un StorableV.! i)) <$> 
                            (tensorScalars <$> ts1)
                       else tensorsFinite <$> ts1
                result = FiniteTensor index2 $ FiniteTensor index1 <$> (_transpose dane)
            -- reconstruct tensor with transposed elements
            in  _mergeScalars result
        -- there is only one index and therefore it cannot be shifted
        | otherwise = t1
    
    -- | Move contravariant indices to lower recursion level
    standardize tens = foldl' (<<<|) tens $ Index.indexName <$> (Index.isContravariant `Prelude.filter` indices tens)

    -- | Filter tensor
    filter _ (Scalar x) = Scalar x
    filter f (SimpleFinite index ts) = 
        let iname = Finite.indexName' index
            ts' = (\i _ -> f iname i) `StorableV.ifilter` ts
        in  SimpleFinite index { Finite.indexSize = StorableV.length ts' } ts'
    filter f (FiniteTensor index ts) = 
        let iname = Finite.indexName' index
            ts' = Multilinear.filter f <$> ((\i _ -> f iname i) `Boxed.ifilter` ts)
            ts'' = 
                (\case 
                    (SimpleFinite _ ts) -> not $ StorableV.null ts
                    (FiniteTensor _ ts) -> not $ Boxed.null ts
                    _ -> error $ "Filter: " ++ tensorOfScalars
                ) `Boxed.filter` ts'
        in  FiniteTensor index { Finite.indexSize = Boxed.length ts'' } ts''
