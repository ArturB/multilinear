{-|
Module      : Multilinear.Generic
Description : Generic implementation of tensor as nested arrays, common for all evaluation models
Copyright   : (c) Artur M. Brodzki, 2018
License     : BSD3
Maintainer  : artur@brodzki.org
Stability   : experimental
Portability : Windows/POSIX

-}

module Multilinear.Generic (
    -- * Generic tensor datatype and its instances
    Tensor(..), 
    -- * Auxiliary functions
    (!), isScalar, isSimple, isFiniteTensor,
    tensorIndex, _standardize, _mergeScalars, _map, _contractedIndices,
    -- * Additional functions
    (.+), (.-), (.*), (+.), (-.), (*.),
    Multilinear.Generic.map, 
    Multilinear.Generic.filter,
    Multilinear.Generic.filterIndex,
    Multilinear.Generic.zipWith
) where

import           Control.DeepSeq
import           Data.Foldable
import           Data.List
import           Data.Maybe
import qualified Data.Set                   as Set
import qualified Data.Vector                as Boxed
import qualified Data.Vector.Unboxed        as Unboxed
import           GHC.Generics
import           Multilinear.Class          as Multilinear
import qualified Multilinear.Index          as Index
import qualified Multilinear.Index.Finite   as Finite

{-| ERROR MESSAGE -}
incompatibleTypes :: String
incompatibleTypes = "Incompatible tensor types!"

{-| ERROR MESSAGE -}
scalarIndices :: String
scalarIndices = "Scalar has no indices!"

{-| ERROR MESSAGE -}
indexNotFound :: String
indexNotFound = "This tensor has not such index!"

{-| ERROR MESSAGE -}
tensorOfScalars :: String
tensorOfScalars = "Tensor construction error! Vector of scalars"

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
        tensorScalars     :: Unboxed.Vector a
    } -> Tensor a
    {-| Finite array of other tensors -}
    FiniteTensor :: {
        {-| Finite index "Mutltilinear.Index.Finite" of tensor -}
        tensorFiniteIndex :: Finite.Index,
        {-| Array of tensors on deeper recursion level -}
        tensorsFinite     :: Boxed.Vector (Tensor a)
    } -> Tensor a
    deriving (Eq, Generic)

{-| Return true if tensor is a scalar -}
{-# INLINE isScalar #-}
isScalar :: Unboxed.Unbox a => Tensor a -> Bool
isScalar x = case x of
    Scalar _ -> True
    _        -> False

{-| Return true if tensor is a simple tensor -}
{-# INLINE isSimple #-}
isSimple :: Unboxed.Unbox a => Tensor a -> Bool
isSimple x = case x of
    SimpleFinite _ _ -> True
    _                -> False

{-| Return True if tensor is a complex tensor -}
{-# INLINE isFiniteTensor #-}
isFiniteTensor :: Unboxed.Unbox a => Tensor a -> Bool
isFiniteTensor x = case x of
    FiniteTensor _ _ -> True
    _                -> False

{-| Return generic tensor index -}
{-# INLINE tensorIndex #-}
tensorIndex :: Unboxed.Unbox a => Tensor a -> Index.TIndex
tensorIndex x = case x of
    Scalar _           -> error scalarIndices
    SimpleFinite i _   -> Index.toTIndex i
    FiniteTensor i _   -> Index.toTIndex i

{-| Returns sample tensor on deeper recursion level.Used to determine some features common for all tensors -}
{-# INLINE firstTensor #-}
firstTensor :: Unboxed.Unbox a => Tensor a -> Tensor a
firstTensor x = case x of
    FiniteTensor _ ts   -> Boxed.head ts
    _                   -> x

{-| Recursive indexing on list tensor
    @t ! i = t[i]@ -}
{-# INLINE (!) #-}
(!) :: Unboxed.Unbox a => Tensor a      -- ^ tensor @t@
    -> Int           -- ^ index @i@
    -> Tensor a      -- ^ tensor @t[i]@
t ! i = case t of
    Scalar _            -> error scalarIndices
    SimpleFinite ind ts -> 
        if i >= Finite.indexSize ind then 
            error ("Index + " ++ show ind ++ " out of bonds!") 
        else Scalar $ ts Unboxed.! i
    FiniteTensor ind ts -> 
        if i >= Finite.indexSize ind then 
            error ("Index + " ++ show ind ++ " out of bonds!") 
        else ts Boxed.! i

-- | NFData instance
instance NFData a => NFData (Tensor a)

-- | Move contravariant indices to lower recursion level
_standardize :: (Num a, Unboxed.Unbox a, NFData a) => Tensor a -> Tensor a
_standardize tens = foldr' f tens $ indices tens
    where 
        f i t = if Index.isContravariant i then 
            t <<<| Index.indexName i 
        else t

-- | Print tensor
instance (
    Unboxed.Unbox a, Show a, Num a, NFData a
    ) => Show (Tensor a) where

    -- merge errors first and then print whole tensor
    show = show' . _standardize
        where
        show' x = case x of
            -- Scalar is showed simply as its value
            Scalar v -> show v
            -- SimpleFinite is shown dependent on its index...
            SimpleFinite index ts -> show index ++ "S: " ++ case index of
                -- If index is contravariant, show tensor components vertically
                Finite.Contravariant _ _ -> "\n" ++ tail (Unboxed.foldl' (\string e -> string ++ "\n  |" ++ show e) "" ts)
                -- If index is covariant or indifferent, show tensor compoments horizontally
                _                        -> "["  ++ tail (Unboxed.foldl' (\string e -> string ++ "," ++ show e) "" ts) ++ "]"
            -- FiniteTensor is shown dependent on its index...
            FiniteTensor index ts -> show index ++ "T: " ++ case index of
                -- If index is contravariant, show tensor components vertically
                Finite.Contravariant _ _ -> "\n" ++ tail (Boxed.foldl' (\string e -> string ++ "\n  |" ++ show e) "" ts)
                -- If index is covariant or indifferent, show tensor compoments horizontally
                _                        -> "["  ++ tail (Boxed.foldl' (\string e -> string ++ "," ++ show e) "" ts) ++ "]"

{-| Merge FiniteTensor of Scalars to SimpleFinite tensor for performance improvement -}
{-# INLINE _mergeScalars #-}
_mergeScalars :: Unboxed.Unbox a => Tensor a -> Tensor a
_mergeScalars x = case x of
    (FiniteTensor index1 ts1) -> case ts1 Boxed.! 0 of
        Scalar _ -> SimpleFinite index1 $ Unboxed.generate (Boxed.length ts1) (\i -> scalarVal (ts1 Boxed.! i))
        _        -> FiniteTensor index1 $ _mergeScalars <$> ts1
    _ -> x

-- | Generic map function, which does not require a,b types to be Num
_map :: (
    Unboxed.Unbox a, Unboxed.Unbox b
    ) => (a -> b)
      -> Tensor a
      -> Tensor b
_map f x = case x of
    -- Mapping scalar simply maps its value
    Scalar v                -> Scalar $ f v
    -- Mapping complex tensor does mapping element by element
    SimpleFinite index ts   -> SimpleFinite index (f `Unboxed.map` ts)
    FiniteTensor index ts   -> FiniteTensor index $ _map f <$> ts

{-| Transpose Vector of Vectors, analogous to Data.List.transpose function. It is assumed, that all vectors on deeper recursion level have the same length.  -}
_transpose :: Boxed.Vector (Boxed.Vector a)  -- ^ Vector of vectors to transpose
           -> Boxed.Vector (Boxed.Vector a)
_transpose v = 
    let outerS = Boxed.length v
        innerS = Boxed.length $ v Boxed.! 0
    in  Boxed.generate innerS (\i -> Boxed.generate outerS $ \j -> v Boxed.! j Boxed.! i)

-- | Contracted indices have to be consumed in result tensor.
_contractedIndices :: 
    Tensor Double -- ^ first tensor to contract
 -> Tensor Double -- ^ second tensor to contract
 -> Set.Set String
_contractedIndices t1 t2 = 
    let iContravariantNames1 = Set.fromList $ Index.indexName <$> (Index.isContravariant `Prelude.filter` indices t1)
        iCovariantNames1 = Set.fromList $ Index.indexName <$> (Index.isCovariant `Prelude.filter` indices t1)

        iContravariantNames2 = Set.fromList $ Index.indexName <$> (Index.isContravariant `Prelude.filter` indices t2)
        iCovariantNames2 = Set.fromList $ Index.indexName <$> (Index.isCovariant `Prelude.filter` indices t2)

    in  -- contracted are indices covariant in the first tensor and contravariant in the second
        Set.intersection iCovariantNames1 iContravariantNames2 `Set.union`
        -- or contravariant in the first tensor and covariant in the second
        Set.intersection iContravariantNames1 iCovariantNames2

-- | zipping error
{-# INLINE zipErr #-}
zipErr :: String         -- ^ zipT function variant where the error occured
       -> Index.TIndex   -- ^ Index of first dot product parameter
       -> Index.TIndex   -- ^ Index of second dot product parameter
       -> Tensor a       -- ^ Erorr message
zipErr variant i1' i2' = error $
    "zipT: " ++ variant ++ " - " ++ incompatibleTypes ++
    " - index1 is " ++ show i1' ++
    " and index2 is " ++ show i2'

-- Multilinear operations
instance (Unboxed.Unbox a, Num a, NFData a) => Multilinear Tensor a where
    -- Generic tensor constructor
    -- If only one upper index is given, generate a SimpleFinite tensor with upper index
    fromIndices [u] [] [s] [] f = 
        SimpleFinite (Finite.Contravariant s [u]) $ Unboxed.generate s $ \x -> f [x] []
  
    -- If only one lower index is given, generate a SimpleFinite tensor with lower index
    fromIndices [] [d] [] [s] f = 
        SimpleFinite (Finite.Covariant s [d]) $ Unboxed.generate s $ \x -> f [] [x]
  
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
    el t1@(SimpleFinite index1 _) (inds,vals) =
            -- zip indices with their given values
        let indvals = zip inds vals
            -- find value for simple tensor index if given
            val = Data.List.find (\(n,_) -> [n] == Index.indexName index1) indvals
            -- if value for current index is given
        in  if isJust val
            -- then get it from current tensor
            then t1 ! snd (fromJust val)
            -- otherwise return whole tensor - no filtering defined
            else t1
    -- finite tensor case
    el t1@(FiniteTensor index1 v1) (inds,vals) =
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
            then el (t1 ! snd (fromJust val)) (inds1,vals1)
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
            Finite.Indifferent _ _   -> (0,0)
        _ -> let (cnvr, covr) = order $ firstTensor x
             in case tensorIndex x of
                Index.Contravariant _ _ -> (cnvr+1,covr)
                Index.Covariant _ _     -> (cnvr,covr+1)
                Index.Indifferent _ _   -> (cnvr,covr)

    -- Get size of tensor index or Left if index is infinite or tensor has no such index
    {-# INLINE size #-}
    size t iname = case t of
        Scalar _             -> error scalarIndices
        SimpleFinite index _ -> 
            if Index.indexName index == iname 
            then Finite.indexSize index 
            else error indexNotFound
        FiniteTensor index _ -> 
            if Index.indexName index == iname
            then Finite.indexSize index
            else size (firstTensor t) iname

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
    transpose (FiniteTensor (Finite.Indifferent count name) ts) =
        FiniteTensor (Finite.Indifferent count name) (Multilinear.transpose <$> ts)

    transpose (SimpleFinite (Finite.Covariant count name) ts) =
        SimpleFinite (Finite.Contravariant count name) ts
    transpose (SimpleFinite (Finite.Contravariant count name) ts) =
        SimpleFinite (Finite.Covariant count name) ts
    transpose (SimpleFinite (Finite.Indifferent count name) ts) =
        SimpleFinite (Finite.Indifferent count name) ts


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
                       then (\un -> Boxed.generate (Unboxed.length un) (\i -> Scalar $ un Unboxed.! i)) <$> 
                            (tensorScalars <$> ts1)
                       else tensorsFinite <$> ts1
            -- reconstruct tensor with transposed elements
            in  _mergeScalars $ FiniteTensor index2 $ FiniteTensor index1 <$> (_transpose dane)
        -- there is only one index and therefore it cannot be shifted
        | otherwise = t1

-- Add scalar right
{-# INLINE (.+) #-}
(.+) :: (
    Unboxed.Unbox a, Num a
    ) => Tensor a 
      -> a 
      -> Tensor a
t .+ x = (+x) `Multilinear.Generic.map` t

-- Subtract scalar right
{-# INLINE (.-) #-}
(.-) :: (
    Unboxed.Unbox a, Num a
    ) => Tensor a 
      -> a 
      -> Tensor a
t .- x = (\p -> p - x) `Multilinear.Generic.map` t

-- Multiplicate by scalar right
{-# INLINE (.*) #-}
(.*) :: (
    Unboxed.Unbox a, Num a
    ) => Tensor a 
      -> a 
      -> Tensor a
t .* x = (*x) `Multilinear.Generic.map` t

-- Add scalar left
{-# INLINE (+.) #-}
(+.) :: (
    Unboxed.Unbox a, Num a
    ) => a 
      -> Tensor a 
      -> Tensor a
x +. t = (x+) `Multilinear.Generic.map` t

-- Subtract scalar left
{-# INLINE (-.) #-}
(-.) :: (
    Unboxed.Unbox a, Num a
    ) => a 
      -> Tensor a 
      -> Tensor a
x -. t = (x-) `Multilinear.Generic.map` t

-- Multiplicate by scalar left
{-# INLINE (*.) #-}
(*.) :: (
    Unboxed.Unbox a, Num a
    ) => a 
      -> Tensor a 
      -> Tensor a
x *. t = (x*) `Multilinear.Generic.map` t

    
{-| Simple mapping -}
{-| @map f t@ returns tensor @t2@ in which @t2[i1,i2,...] = f t[i1,i2,...]@ -}
{-# INLINE map #-}
map :: (
    Unboxed.Unbox a, Unboxed.Unbox b
    ) => (a -> b) 
      -> Tensor a 
      -> Tensor b
map f x = case x of
    -- Mapping scalar simply maps its value
    Scalar v                -> Scalar $ f v
    -- Mapping complex tensor does mapping element by element
    SimpleFinite index ts   -> SimpleFinite index (f `Unboxed.map` ts)
    FiniteTensor index ts   -> FiniteTensor index $ Multilinear.Generic.map f <$> ts

{-| Filtering tensor. 
    Filtering multi-dimensional arrray may be dangerous, as we always assume, 
    that on each recursion level, all tensors have the same size (length). 
    To disable invalid filters, filtering is done over indices, not tensor elements. 
    Filter function takes and index name and index value and if it returns True, this index value remains in result tensor. 
    This allows to remove whole columns or rows of eg. a matrix: 
        filter (\i n -> i /= "a" || i > 10) filters all rows of "a" index (because if i /= "a", filter returns True)
        and for "a" index filter elements with index value <= 10
    But this disallow to remove particular matrix element. 
    If for some index all elements are removed, the index itself is removed from tensor. -}
filter :: (
    Unboxed.Unbox a
    ) => (String -> Int -> Bool) -- ^ filter function
      -> Tensor a                -- ^ tensor to filter
      -> Tensor a
filter _ (Scalar x) = Scalar x
filter f (SimpleFinite index ts) = 
    let iname = Finite.indexName' index
        ts' = (\i _ -> f iname i) `Unboxed.ifilter` ts
    in  SimpleFinite index { Finite.indexSize = Unboxed.length ts' } ts'
filter f (FiniteTensor index ts) = 
    let iname = Finite.indexName' index
        ts' = Multilinear.Generic.filter f <$> ((\i _ -> f iname i) `Boxed.ifilter` ts)
        ts'' = 
            (\case 
                (SimpleFinite _ ts) -> not $ Unboxed.null ts
                (FiniteTensor _ ts) -> not $ Boxed.null ts
                _ -> error $ "Filter: " ++ tensorOfScalars
            ) `Boxed.filter` ts'
    in  FiniteTensor index { Finite.indexSize = Boxed.length ts'' } ts''

{-| Filtering one index of tensor. -}
filterIndex :: (
    Unboxed.Unbox a
    ) => String        -- ^ Index name to filter
      -> (Int -> Bool) -- ^ filter function
      -> Tensor a      -- ^ tensor to filter
      -> Tensor a
filterIndex iname f = Multilinear.Generic.filter (\i n -> i /= iname || f n)

{-| Zip tensors with binary combinator -}
zipWith :: (
    Unboxed.Unbox a, Unboxed.Unbox b, Unboxed.Unbox c
    ) => (a -> b -> c) 
      -> Tensor a 
      -> Tensor b 
      -> Tensor c
-- Zipping two Scalars simply combines their values 
zipWith f (Scalar x1) (Scalar x2) = Scalar $ f x1 x2
-- zipping complex tensor with scalar 
zipWith f t (Scalar x) = (`f` x) `_map` t
-- zipping scalar with complex tensor
zipWith f (Scalar x) t = (x `f`) `_map` t
-- Two simple tensors case
zipWith f (SimpleFinite index1 v1) (SimpleFinite index2 v2) = 
    if index1 == index2 then 
        SimpleFinite index1 $ Unboxed.zipWith f v1 v2 
    else zipErr "simple-simple" (Index.toTIndex index1) (Index.toTIndex index2)
--Two finite tensors case
zipWith f (FiniteTensor index1 v1) (FiniteTensor index2 v2)     = 
    if index1 == index2 then 
        FiniteTensor index1 $ Boxed.zipWith (Multilinear.Generic.zipWith f) v1 v2 
    else zipErr "finite-finite" (Index.toTIndex index1) (Index.toTIndex index2)
-- Finite and simple tensor case
zipWith f (FiniteTensor index1 v1) (SimpleFinite index2 v2)     = 
    if index1 == index2 then 
        let f' t s = (`f` s) `_map` t
        in  FiniteTensor index1 $ Boxed.generate (Finite.indexSize index1) (\i -> f' (v1 Boxed.! i) (v2 Unboxed.! i)) 
    else zipErr "finite-simple" (Index.toTIndex index1) (Index.toTIndex index2)
-- Simple and finite tensor case
zipWith f (SimpleFinite index1 v1) (FiniteTensor index2 v2)     = 
    if index1 == index2 then 
        let f' s t = (s `f`) `_map` t
        in  FiniteTensor index1 $ Boxed.generate (Finite.indexSize index1) (\i -> f' (v1 Unboxed.! i) (v2 Boxed.! i))
    else zipErr "simple-finite" (Index.toTIndex index1) (Index.toTIndex index2)
