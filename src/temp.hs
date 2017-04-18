



-- Tensors can be added, subtracted and multiplicated
instance (
    Eq i, Show i, Integral i,
    Eq a, Show a, Num a, Bits a
    ) => Num (Tensor c i a) where

    -- Adding - element by element
    Scalar x1 + Scalar x2 = Scalar $ x1 + x2
    Scalar x + t = (x+) <$> t
    t + Scalar x = (+x) <$> t
    t1@(FiniteTensor index1 v1) + t2@(FiniteTensor index2 v2)
        | index1 == index2 = Tensor index1 $ Data.List.zipWith (+) v1 v2
        | index1 `Data.List.elem` indices t2 =
            let t1' = t1 |>>> indexName index1
                t2' = t2 |>>> indexName index1
            in  t1' + t2'
        | otherwise = Tensor index1 [t + t2 | t <- v1]
    Err msg + _ = Err msg
    _ + Err msg = Err msg

    -- Subtracting - element by element
    Scalar x1 - Scalar x2 = Scalar $ x1 - x2
    Scalar x - t = (\e -> x - e) <$> t
    t - Scalar x = (\e -> e - x) <$> t
    t1@(FiniteTensor index1 v1) - t2@(FiniteTensor index2 v2)
        | index1 == index2 = Tensor index1 $ Data.List.zipWith (-) v1 v2
        | index1 `Data.List.elem` indices t2 =
            let t1' = t1 |>>> indexName index1
                t2' = t2 |>>> indexName index1
            in  t1' - t2'
        | otherwise = Tensor index1 [t - t2 | t <- v1]
    Err msg - _ = Err msg
    _ - Err msg = Err msg

    -- Multiplicating is treated as tensor product
    -- Tensor product applies Einstein summation convention
    -- Two scalars are multiplicated by their values
    Scalar x1 * Scalar x2 = Scalar $ x1 * x2
    -- Multiplicate by scalar is simply a map
    Scalar x1 * t = (x1*) <$> t
    t * Scalar x2 = (*x2) <$> t
    -- Two tensors may be contracted or multiplicated elem by elem
    t1@(FiniteTensor index1 ts1) * t2@(FiniteTensor index2 _)
        | indexName index1 == indexName index2 = t1 `dot` t2
        | indexName index1 `Data.List.elem` indicesNames t2 =
            let t1' = t1 |>>> indexName index1
                t2' = t2 |>>> indexName index1
            in  t1' * t2'
        | otherwise = Tensor index1 $ (* t2) <$> ts1
        where
        -- Contraction of covariant and contravariant index
        FiniteTensor i1@(Covariant count1 _) ts1' `dot` FiniteTensor i2@(Contravariant count2 _) ts2'
            | count1 == count2 = sum $ Data.List.zipWith (*) ts1' ts2'
            | otherwise = contractionErr i1 i2
        -- Contraction of contravariant and covariant index
        FiniteTensor i1@(Contravariant count1 _) ts1' `dot` FiniteTensor i2@(Covariant count2 _) ts2'
            | count1 == count2 = sum $ Data.List.zipWith (*) ts1' ts2'
            | otherwise = contractionErr i1 i2
        _ `dot` _ = Err "Cannot compute a dot product!"
        contractionErr i1' i2' = Err $
                "Tensor product: " ++ incompatibleTypes ++
                " - index1 is " ++ show i1' ++
                " and index2 is " ++ show i2'
    -- Multiplicating by error simply pushed this error forward
    Err msg * _ = Err msg
    _ * Err msg = Err msg

    -- Absolute value - element by element
    abs (Scalar x)        = Scalar $ abs x
    abs (FiniteTensor index ts) = Tensor index (abs <$> ts)
    abs (Err msg)         = Err msg

    -- Signum operation - element by element
    signum (Scalar x)        = Scalar $ signum x
    signum (FiniteTensor index ts) = Tensor index (signum <$> ts)
    signum (Err msg)         = Err msg

    -- Simple integer can be conveted to Scalar
    fromInteger x = Scalar $ fromInteger x

-- Tensors can be divided by each other
instance (
    Eq i, Show i, Integral i,
    Eq a, Show a, Fractional a, Bits a
    ) => Fractional (Tensor c i a) where

    -- Scalar division return result of division of its values
    Scalar x1 / Scalar x2 = Scalar $ x1 / x2
    -- Tensor and scalar are divided value by value
    Scalar x1 / t2 = (x1 /) <$> t2
    t1 / Scalar x2 = (/ x2) <$> t1
    -- Two complex tensors cannot be simply divided
    t1@(FiniteTensor _ _) / t2@(FiniteTensor _ _) = Err $
         "(/): " ++ incompatibleTypes ++
        " tensor1 has type " ++ show (indices t1) ++
        " and tensor2 has type " ++ show (indices t2)
    Err msg / _ = Err msg
    _ / Err msg = Err msg

    -- Scalar can be generated from rational number
    fromRational x = Scalar $ fromRational x

-- Real-number functions on tensors.
-- Function of tensor is a tensor of function of its elements
-- E.g. exp [1,2,3,4] = [exp 1, exp2, exp3, exp4]
instance (
    Eq i, Show i, Integral i,
    Eq a, Show a, Floating a, Bits a
    ) => Floating (Tensor c i a) where

    pi = Scalar pi

    exp (Scalar x)        = Scalar $ exp x
    exp (FiniteTensor index ts) = Tensor index (exp <$> ts)
    exp (Err msg)         = Err msg

    log (Scalar x)        = Scalar $ log x
    log (FiniteTensor index ts) = Tensor index (log <$> ts)
    log (Err msg)         = Err msg

    sin (Scalar x)        = Scalar $ sin x
    sin (FiniteTensor index ts) = Tensor index (sin <$> ts)
    sin (Err msg)         = Err msg

    cos (Scalar x)        = Scalar $ cos x
    cos (FiniteTensor index ts) = Tensor index (cos <$> ts)
    cos (Err msg)         = Err msg

    asin (Scalar x)        = Scalar $ asin x
    asin (FiniteTensor index ts) = Tensor index (asin <$> ts)
    asin (Err msg)         = Err msg

    acos (Scalar x)        = Scalar $ acos x
    acos (FiniteTensor index ts) = Tensor index (acos <$> ts)
    acos (Err msg)         = Err msg

    atan (Scalar x)        = Scalar $ atan x
    atan (FiniteTensor index ts) = Tensor index (atan <$> ts)
    atan (Err msg)         = Err msg

    sinh (Scalar x)        = Scalar $ sinh x
    sinh (FiniteTensor index ts) = Tensor index (sinh <$> ts)
    sinh (Err msg)         = Err msg

    cosh (Scalar x)        = Scalar $ cosh x
    cosh (FiniteTensor index ts) = Tensor index (cosh <$> ts)
    cosh (Err msg)         = Err msg

    asinh (Scalar x)        = Scalar $ asinh x
    asinh (FiniteTensor index ts) = Tensor index (asinh <$> ts)
    asinh (Err msg)         = Err msg

    acosh (Scalar x)        = Scalar $ acosh x
    acosh (FiniteTensor index ts) = Tensor index (acosh <$> ts)
    acosh (Err msg)         = Err msg

    atanh (Scalar x)        = Scalar $ atanh x
    atanh (FiniteTensor index ts) = Tensor index (atanh <$> ts)
    atanh (Err msg)         = Err msg

-- Multilinear operations
instance (
    Eq i, Show i, Integral i,
    Eq a, Show a, Num a, Bits a
    ) => Multilinear (Tensor c i) a where

    -- Add scalar left
    x .+ t = (x+) <$> t

    -- Subtract scalar left
    x .- t = (x-) <$> t

    -- Multiplicate by scalar left
    x .* t = (x*) <$> t

    -- Add scalar right
    t +. x = (+x) <$> t

    -- Subtract scalar right
    t -. x = (\p -> p - x) <$> t

    -- Multiplicate by scalar right
    t *. x = (*x) <$> t

    -- Get tensor order [ (contravariant,covariant)-type ]
    order (Scalar _) = (0,0)
    order (FiniteTensor (Contravariant _ _) t) = (cnvr+1,covr)
        where (cnvr,covr) = order $ Data.List.head t
    order (FiniteTensor (Covariant _ _) t) = (cnvr,covr+1)
        where (cnvr,covr) = order $ Data.List.head t
    order (FiniteTensor (Indifferent _ _) t) = (cnvr,covr)
        where (cnvr,covr) = order $ Data.List.head t
    order (Err _) = (-1,-1)

    -- Rename tensor index
    rename (Scalar x) _ _ = Scalar x
    rename (FiniteTensor i@(Contravariant count name) ts) before after
        | name == before = Tensor (Contravariant count after) $ (\t' -> rename t' before after) <$> ts
        | otherwise = Tensor i $ (\t' -> rename t' before after) <$> ts
    rename (FiniteTensor i@(Covariant count name) ts) before after
        | name == before = Tensor (Covariant count after) $ (\t' -> rename t' before after) <$> ts
        | otherwise = Tensor i $ (\t' -> rename t' before after) <$> ts
    rename (FiniteTensor i@(Indifferent count name) ts) before after
        | name == before = Tensor (Indifferent count after) $ (\t' -> rename t' before after) <$> ts
        | otherwise = Tensor i $ (\t' -> rename t' before after) <$> ts
    rename (Err msg) _ _ = Err msg

    -- Raise an index
    Scalar x /\ _ = Scalar x
    FiniteTensor index ts /\ n
        | indexName index == n =
            Tensor (Contravariant (indexSize index) n) [t /\ n | t <- ts]
        | otherwise =
            Tensor index [t /\ n | t <- ts]
    Err msg /\ _ = Err msg

    -- Lower an index
    Scalar x \/ _ = Scalar x
    FiniteTensor index ts \/ n
        | indexName index == n =
            Tensor (Covariant (indexSize index) n) [t /\ n | t <- ts]
        | otherwise =
            Tensor index [t /\ n | t <- ts]
    Err msg \/ _ = Err msg

    {-| Transpose a tensor (switch all indices types) -}
    transpose (Scalar x) = Scalar x
    transpose (FiniteTensor (Covariant count name) ts) =
        Tensor (Contravariant count name) (Multilinear.transpose <$> ts)
    transpose (FiniteTensor (Contravariant count name) ts) =
        Tensor (Covariant count name) (Multilinear.transpose <$> ts)
    transpose (FiniteTensor (Indifferent count name) ts) =
        Tensor (Indifferent count name) (Multilinear.transpose <$> ts)
    transpose (Err msg) = Err msg

    {-| Mapping with indices. -}
    iMap f t = iMap' t zeroList
        where
        zeroList = 0:zeroList

        iMap' (Scalar x) inds        = Scalar $ f inds x
        iMap' (FiniteTensor index ts) inds = Tensor index [iMap' (fst tind) $ inds ++ [snd tind] | tind <- zip ts [0..] ]
        iMap' (Err msg) _            = Err msg

    {-| Concatenation of two tensor with given index or by creating a new one -}
    augment t1@(FiniteTensor _ _) t2@(FiniteTensor _ _) ind =
        if indices t1 == indices t2
        then
            let t1' = t1 <<<| ind
                t2' = t2 <<<| ind
            in  Tensor (tensorIndex t1') $ tensorData t1' ++ tensorData t2'
        else Err "Tensors are not equivalent!"
    augment (Err msg) _ _ = Err msg
    augment _ (Err msg) _ = Err msg
    augment (Scalar _) _ _ = Err "Scalars cannot be augmented!"
    augment _ (Scalar _) _ = Err "Cannot augment by scalar!"

    {-| Shift tensor index right -}
    {-| Moves given index one level deeper in recursion -}
    Err msg |>> _  = Err msg
    Scalar x |>> _ = Scalar x
    t1@(FiniteTensor index1 ts1) |>> ind
        | Data.List.length (indices t1) > 1 && indexName index1 /= ind =
            Tensor index1 [t |>> ind | t <- ts1]
        | Data.List.length (indices t1) > 1 && indexName index1 == ind =
            let index2 = tensorIndex (Data.List.head ts1)
            in Tensor index2 [Tensor index1 [tensorData (ts1 !! fromIntegral j) !! fromIntegral i
                | j <- [0 .. indexSize index1 - 1]]
                | i <- [0 .. indexSize index2 - 1]]
        | otherwise = t1




