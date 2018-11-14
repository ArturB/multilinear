{-|
Module      : Multilinear.Generic.AsArray
Description : Generic array tensor
Copyright   : (c) Artur M. Brodzki, 2018
License     : BSD3
Maintainer  : artur@brodzki.org
Stability   : experimental
Portability : Windows/POSIX

- This module contains generic implementation of tensor defined as nested arrays

-}

module Multilinear.Generic (
    Tensor(..), (!),
    isScalar, isSimple, isFiniteTensor,
    dot, _elemByElem, contractionErr, tensorIndex, _standardize
) where

import           Control.DeepSeq
import           Data.Foldable
import           Data.List
import           Data.Maybe
import qualified Data.Vector                as Boxed
import qualified Data.Vector.Unboxed        as Unboxed
import           GHC.Generics
import           Multilinear.Class          as Multilinear
import qualified Multilinear.Index          as Index
import qualified Multilinear.Index.Finite   as Finite

{-| ERROR MESSAGES -}
incompatibleTypes :: String
incompatibleTypes = "Incompatible tensor types!"

scalarIndices :: String
scalarIndices = "Scalar has no indices!"

indexNotFound :: String
indexNotFound = "This tensor has not such index!"

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

{-| Return True if tensor has no elements -}
{-# INLINE isEmptyTensor #-}
isEmptyTensor :: Unboxed.Unbox a => Tensor a -> Bool
isEmptyTensor x = case x of
    Scalar _            -> False
    SimpleFinite _ ts   -> Unboxed.null ts
    FiniteTensor _ ts   -> Boxed.null ts

{-| Returns sample element of the tensor. Used to determine some features common for all elements, like bit-qualities. -}
{-# INLINE firstElem #-}
firstElem :: Unboxed.Unbox a => Tensor a -> a
firstElem x = case x of
    Scalar val          -> val
    SimpleFinite _ ts   -> Unboxed.head ts
    FiniteTensor _ ts   -> firstElem $ Boxed.head ts

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

-- NFData instance
instance NFData a => NFData (Tensor a)

-- move contravariant indices to lower recursion level
_standardize :: (Num a, Unboxed.Unbox a) => Tensor a -> Tensor a
_standardize tens = foldr' f tens $ indices tens
    where 
        f i t = if Index.isContravariant i then 
            t <<<| Index.indexName i 
        else t

-- Print tensor
instance (
    Unboxed.Unbox a, Show a, Num a
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

-- Tensors can be compared lexigographically
-- Allowes to put tensors in typical ordered containers
instance (
    Ord a, Unboxed.Unbox a
    ) => Ord (Tensor a) where

    {-# INLINE (<=) #-}
    -- Scalar is smaller than any complex tensor
    -- Two scalars are compared by they values
    Scalar x1 <= Scalar x2 = x1 <= x2
    Scalar _ <= _ = True
    _ <= Scalar _ = False
    -- Complex tensors are compared lexigographically
    SimpleFinite _ ts1 <= SimpleFinite _ ts2     = ts1 <= ts2
    FiniteTensor _ ts1 <= FiniteTensor _ ts2     = ts1 <= ts2
    FiniteTensor _ _ <= SimpleFinite _ _         = False
    SimpleFinite _ _ <= FiniteTensor _ _         = True

{-| Merge FiniteTensor of Scalars to SimpleFinite tensor for performance improvement -}
{-# INLINE _mergeScalars #-}
_mergeScalars :: Unboxed.Unbox a => Tensor a -> Tensor a
_mergeScalars x = case x of
    (FiniteTensor index1 ts1) -> case ts1 Boxed.! 0 of
        Scalar _ -> SimpleFinite index1 $ Unboxed.generate (Boxed.length ts1) (\i -> scalarVal (ts1 Boxed.! i))
        _        -> FiniteTensor index1 $ _mergeScalars <$> ts1
    _ -> x

{-| Apply a tensor operator (here denoted by (+) ) elem by elem, trying to connect as many common indices as possible -}
{-# INLINE _elemByElem' #-}
_elemByElem' :: (Num a, Unboxed.Unbox a)
             => Tensor a                            -- ^ First argument of operator
             -> Tensor a                            -- ^ Second argument of operator
             -> (a -> a -> a)                       -- ^ Function on tensor elements if indices are different
             -> (Tensor a -> Tensor a -> Tensor a)  -- ^ Tensor operator called if indices are the same
             -> Tensor a                            -- ^ Result tensor

-- @Scalar x + Scalar y = Scalar x + y@
_elemByElem' (Scalar x1) (Scalar x2) f _ = Scalar $ f x1 x2
-- @Scalar x + Tensor t[i] = Tensor r[i] | r[i] = x + t[i]@
_elemByElem' (Scalar x) t f _ = (x `f`) `Multilinear.map` t
-- @Tensor t[i] + Scalar x = Tensor r[i] | r[i] = t[i] + x@
_elemByElem' t (Scalar x) f _ = (`f` x) `Multilinear.map` t

-- Two simple tensors case
_elemByElem' t1@(SimpleFinite index1 v1) t2@(SimpleFinite index2 _) f op
    | Index.indexName index1 == Index.indexName index2 = op t1 t2
    | otherwise = FiniteTensor index1 $ 
        Boxed.generate (Unboxed.length v1) 
            (\i -> (\x -> f x `Multilinear.map` t2) (v1 Unboxed.! i))

-- Two finite tensors case
_elemByElem' t1@(FiniteTensor index1 v1) t2@(FiniteTensor index2 v2) f op
    | Index.indexName index1 == Index.indexName index2 = op t1 t2
    | Index.indexName index1 `Data.List.elem` indicesNames t2 =
        FiniteTensor index2 $ (\x -> _elemByElem' t1 x f op) <$> v2
    | otherwise = FiniteTensor index1 $ (\x -> _elemByElem' x t2 f op) <$> v1

-- Simple and finite tensor case
_elemByElem' t1@(SimpleFinite index1 _) t2@(FiniteTensor index2 v2) f op
    | Index.indexName index1 == Index.indexName index2 = op t1 t2
    | otherwise = FiniteTensor index2 $ (\x -> _elemByElem' t1 x f op) <$> v2

-- Finite and simple tensor case
_elemByElem' t1@(FiniteTensor index1 v1) t2@(SimpleFinite index2 _) f op
    | Index.indexName index1 == Index.indexName index2 = op t1 t2
    | otherwise = FiniteTensor index1 $ (\x -> _elemByElem' x t2 f op) <$> v1

{-| Apply a tensor operator elem by elem -}
{-# INLINE _elemByElem #-}
_elemByElem :: (Num a, Unboxed.Unbox a)
            => Tensor a                             -- ^ First argument of operator
            -> Tensor a                             -- ^ Second argument of operator
            -> (a -> a -> a)                        -- ^ Operator on tensor elements if indices are different
            -> (Tensor a -> Tensor a -> Tensor a)   -- ^ Tensor operator called if indices are the same
            -> Tensor a                             -- ^ Result tensor
_elemByElem t1 t2 f op = 
    let commonIndices = filter (`Data.List.elem` indicesNames t2) $ indicesNames t1
        t1' = foldl' (|>>>) t1 commonIndices
        t2' = foldl' (|>>>) t2 commonIndices
    in _mergeScalars $ _elemByElem' t1' t2' f op

-- Zipping two tensors with a combinator, assuming they have the same indices
{-# INLINE zipT #-}
zipT :: (Num a, Unboxed.Unbox a)
      => (Tensor a -> Tensor a -> Tensor a)   -- ^ Two tensors combinator
      -> (Tensor a -> a -> Tensor a)          -- ^ Tensor and scalar combinator
      -> (a -> Tensor a -> Tensor a)          -- ^ Scalar and tensor combinator
      -> (a -> a -> a)                        -- ^ Two scalars combinator
      -> Tensor a                             -- ^ First tensor to zip
      -> Tensor a                             -- ^ Second tensor to zip
      -> Tensor a                             -- ^ Result tensor

-- Two simple tensors case
zipT _ _ _ f (SimpleFinite index1 v1) (SimpleFinite index2 v2) = 
    if index1 == index2 then 
        SimpleFinite index1 $ Unboxed.zipWith f v1 v2 
    else error incompatibleTypes

--Two finite tensors case
zipT f _ _ _ (FiniteTensor index1 v1) (FiniteTensor index2 v2) = 
    if index1 == index2 then 
        FiniteTensor index1 $ Boxed.zipWith f v1 v2 
    else error incompatibleTypes

-- Finite and simple tensor case
zipT _ f _ _ (FiniteTensor index1 v1) (SimpleFinite index2 v2) = 
    if index1 == index2 then 
        FiniteTensor index1 $ 
            Boxed.generate (Boxed.length v1) (\i -> f (v1 Boxed.! i) (v2 Unboxed.! i)) 
    else error incompatibleTypes

-- Simple and finite tensor case
zipT _ _ f _ (SimpleFinite index1 v1) (FiniteTensor index2 v2) = 
    if index1 == index2 then 
        FiniteTensor index1 $ 
            Boxed.generate (Unboxed.length v1) (\i -> f (v1 Unboxed.! i) (v2 Boxed.! i)) 
    else error incompatibleTypes

-- Zipping something with scalar is impossible
zipT _ _ _ _ _ _ = error scalarIndices

-- dot product of two tensors
{-# INLINE dot #-}
dot :: (Num a, Unboxed.Unbox a)
      => Tensor a  -- ^ First dot product argument
      -> Tensor a  -- ^ Second dot product argument
      -> Tensor a  -- ^ Resulting dot product

-- Two simple tensors product
dot (SimpleFinite i1@(Finite.Covariant count1 _) ts1') (SimpleFinite i2@(Finite.Contravariant count2 _) ts2')
    | count1 == count2 = 
        Scalar $ Unboxed.sum $ Unboxed.zipWith (*) ts1' ts2'
    | otherwise = contractionErr (Index.toTIndex i1) (Index.toTIndex i2)

-- Two finite tensors product
dot (FiniteTensor i1@(Finite.Covariant count1 _) ts1') (FiniteTensor i2@(Finite.Contravariant count2 _) ts2')
    | count1 == count2 = Boxed.sum $ Boxed.zipWith (*) ts1' ts2'
    | otherwise = contractionErr (Index.toTIndex i1) (Index.toTIndex i2)

--  Other cases cannot happen!
dot t1' t2' = contractionErr (tensorIndex t1') (tensorIndex t2')

-- contraction error
{-# INLINE contractionErr #-}
contractionErr :: Index.TIndex   -- ^ Index of first dot product parameter
               -> Index.TIndex   -- ^ Index of second dot product parameter
               -> Tensor a       -- ^ Erorr message

contractionErr i1' i2' = error $
    "Tensor product: " ++ incompatibleTypes ++
    " - index1 is " ++ show i1' ++
    " and index2 is " ++ show i2'

-- Tensors can be added, subtracted and multiplicated
instance (Unboxed.Unbox a, Num a) => Num (Tensor a) where

    -- Adding - element by element
    {-# INLINE (+) #-}
    t1 + t2 = _elemByElem t1 t2 (+) $ zipT (+) (.+) (+.) (+)

    -- Subtracting - element by element
    {-# INLINE (-) #-}
    t1 - t2 = _elemByElem t1 t2 (-) $ zipT (-) (.-) (-.) (-)

    -- Multiplicating is treated as tensor product
    -- Tensor product applies Einstein summation convention
    {-# INLINE (*) #-}
    t1 * t2 = _elemByElem t1 t2 (*) dot

    -- Absolute value - element by element
    {-# INLINE abs #-}
    abs t = abs `Multilinear.map` t

    -- Signum operation - element by element
    {-# INLINE signum #-}
    signum t = signum `Multilinear.map` t

    -- Simple integer can be conveted to Scalar
    {-# INLINE fromInteger #-}
    fromInteger x = Scalar $ fromInteger x

-- Tensors can be divided by each other
instance (Unboxed.Unbox a, Fractional a) => Fractional (Tensor a) where

    {-# INLINE (/) #-}
    -- Scalar division return result of division of its values
    Scalar x1 / Scalar x2 = Scalar $ x1 / x2
    -- Tensor and scalar are divided value by value
    Scalar x1 / t2 = (x1 /) `Multilinear.map` t2
    t1 / Scalar x2 = (/ x2) `Multilinear.map` t1
    -- Two complex tensors cannot be (for now) simply divided
    -- // TODO - tensor division and inversion
    _ / _ = error "TODO"

    -- A scalar can be generated from rational number
    {-# INLINE fromRational #-}
    fromRational x = Scalar $ fromRational x

-- Real-number functions on tensors.
-- Function of tensor is tensor of function of its elements
-- E.g. exp [1,2,3,4] = [exp 1, exp2, exp3, exp4]
instance (Unboxed.Unbox a, Floating a) => Floating (Tensor a) where

    {-| PI number -}
    {-# INLINE pi #-}
    pi = Scalar pi

    {-| Exponential function. (exp t)[i] = exp( t[i] ) -}
    {-# INLINE exp #-}
    exp t = exp `Multilinear.map` t

    {-| Natural logarithm. (log t)[i] = log( t[i] ) -}
    {-# INLINE log #-}
    log t = log `Multilinear.map` t

    {-| Sinus. (sin t)[i] = sin( t[i] ) -}
    {-# INLINE sin #-}
    sin t = sin `Multilinear.map` t

    {-| Cosinus. (cos t)[i] = cos( t[i] ) -}
    {-# INLINE cos #-}
    cos t = cos `Multilinear.map` t

    {-| Inverse sinus. (asin t)[i] = asin( t[i] ) -}
    {-# INLINE asin #-}
    asin t = asin `Multilinear.map` t

    {-| Inverse cosinus. (acos t)[i] = acos( t[i] ) -}
    {-# INLINE acos #-}
    acos t = acos `Multilinear.map` t

    {-| Inverse tangent. (atan t)[i] = atan( t[i] ) -}
    {-# INLINE atan #-}
    atan t = atan `Multilinear.map` t

    {-| Hyperbolic sinus. (sinh t)[i] = sinh( t[i] ) -}
    {-# INLINE sinh #-}
    sinh t = sinh `Multilinear.map` t

    {-| Hyperbolic cosinus. (cosh t)[i] = cosh( t[i] ) -}
    {-# INLINE cosh #-}
    cosh t = cosh `Multilinear.map` t

    {-| Inverse hyperbolic sinus. (asinh t)[i] = asinh( t[i] ) -}
    {-# INLINE asinh #-}
    asinh t = acosh `Multilinear.map` t

    {-| Inverse hyperbolic cosinus. (acosh t)[i] = acosh (t[i] ) -}
    {-# INLINE acosh #-}
    acosh t = acosh `Multilinear.map` t

    {-| Inverse hyperbolic tangent. (atanh t)[i] = atanh( t[i] ) -}
    {-# INLINE atanh #-}
    atanh t = atanh `Multilinear.map` t

-- Multilinear operations
instance (Unboxed.Unbox a, Num a) => Multilinear Tensor a where

    -- Add scalar right
    {-# INLINE (.+) #-}
    t .+ x = (+x) `Multilinear.map` t

    -- Subtract scalar right
    {-# INLINE (.-) #-}
    t .- x = (\p -> p - x) `Multilinear.map` t

    -- Multiplicate by scalar right
    {-# INLINE (.*) #-}
    t .* x = (*x) `Multilinear.map` t

    -- Add scalar left
    {-# INLINE (+.) #-}
    x +. t = (x+) `Multilinear.map` t

    -- Subtract scalar left
    {-# INLINE (-.) #-}
    x -. t = (x-) `Multilinear.map` t

    -- Multiplicate by scalar left
    {-# INLINE (*.) #-}
    x *. t = (x*) `Multilinear.map` t

    -- Two tensors sum
    {-# INLINE (.+.) #-}
    t1 .+. t2 = _elemByElem t1 t2 (+) $ zipT (+) (.+) (+.) (+)

    -- Two tensors difference
    {-# INLINE (.-.) #-}
    t1 .-. t2 = _elemByElem t1 t2 (-) $ zipT (+) (.+) (+.) (+)

    -- Tensor product
    {-# INLINE (.*.) #-}
    t1 .*. t2 = _elemByElem t1 t2 (+) dot

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
    {-# INLINE shiftRight #-}
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
                       then (\un -> Boxed.generate (Unboxed.length un) (\i -> Scalar $ un Unboxed.! i)) <$> (tensorScalars <$> ts1)
                       else tensorsFinite <$> ts1
                -- Convert to list
                daneList = Boxed.toList <$> Boxed.toList dane
                -- and transpose tensor data (standard function available only for list)
                transposedList = Data.List.transpose daneList
                -- then reconvert to vector again
                transposed = Boxed.fromList <$> Boxed.fromList transposedList
            -- and reconstruct tensor with transposed elements
            in  _mergeScalars$ FiniteTensor index2 $ FiniteTensor index1 <$> transposed
        -- there is only one index and therefore it cannot be shifted
        | otherwise = t1
    
    {-# INLINE map #-}
    map f x = case x of
        -- Mapping scalar simply maps its value
        Scalar v                -> Scalar $ f v
        -- Mapping complex tensor does mapping element by element
        SimpleFinite index ts   -> SimpleFinite index (f `Unboxed.map` ts)
        FiniteTensor index ts   -> FiniteTensor index $ Multilinear.map f <$> ts

{-| List allows for random access to tensor elements -}
instance (Unboxed.Unbox a, Num a) => Accessible Tensor a where

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
