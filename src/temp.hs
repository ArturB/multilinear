{-Scalar x1 + Scalar x2 = Scalar $ x1 + x2
    Scalar x + t = (x+) <$> t
    t + Scalar x = (+x) <$> t
    t1@(FiniteTensor index1 v1) + t2@(FiniteTensor index2 v2)
        | index1 == index2 = FiniteTensor index1 $ (+) <$> v1 <*> v2
        | indexName index1 `Data.List.elem` indicesNames t2 =
            FiniteTensor index2 $ (t1+) <$> tensorData t2
        | otherwise = FiniteTensor index1 $ (+t2) <$> tensorData t1
    t1@(SimpleFinite index1 v1) + t2@(SimpleFinite index2 v2)
        | index1 == index2 = SimpleFinite index1 $ (+) <$> v1 <*> v2
        | indexName index1 `Data.List.elem` indicesNames t2 =
            FiniteTensor index2 $ (t1+) <$> tensorData t2
        | otherwise = FiniteTensor index1 $ (+t2) <$> tensorData t1
    t1@(SimpleFinite index1 v1) + t2@(FiniteTensor index2 v2)
        | index1 == index2 = FiniteTensor index1 $ (.+) <$> v1 <*> v2
        | indexName index1 `Data.List.elem` indicesNames t2 =
            FiniteTensor index2 $ (t1+) <$> tensorData t2
        | otherwise = FiniteTensor index1 $ (+t2) <$> tensorData t1
    t1@(FiniteTensor index1 v1) + t2@(SimpleFinite index2 v2)
        | index1 == index2 = FiniteTensor index1 $ (+.) <$> v1 <*> v2
        | indexName index1 `Data.List.elem` indicesNames t2 =
            FiniteTensor index2 $ (t1+) <$> tensorData t2
        | otherwise = FiniteTensor index1 $ (+t2) <$> tensorData t1
    Err msg + _ = Err msg
    _ + Err msg = Err msg-}





    {-Scalar x1 - Scalar x2 = Scalar $ x1 - x2
    Scalar x - t = (\e -> x - e) <$> t
    t - Scalar x = (\e -> e - x) <$> t
    t1@(FiniteTensor index1 v1) - t2@(FiniteTensor index2 v2)
        | index1 == index2 = FiniteTensor index1 $ (-) <$> v1 <*> v2
        | indexName index1 `Data.List.elem` indicesNames t2 =
            FiniteTensor index2 $ (t1-) <$> tensorData t2
        | otherwise = FiniteTensor index1 $ (\x -> x - t2) <$> tensorData t1
    Err msg - _ = Err msg
    _ - Err msg = Err msg-}

{-Scalar x1 * Scalar x2 = Scalar $ x1 * x2
    -- Multiplicate by scalar is simply a map
    Scalar x1 * t = (x1*) <$> t
    t * Scalar x2 = (*x2) <$> t
    -- Two tensors may be contracted or multiplicated elem by elem
    t1@(FiniteTensor index1 _) * t2@(FiniteTensor index2 _)
        | indexName index1 == indexName index2 = t1 `dot` t2
        | indexName index1 `Data.List.elem` indicesNames t2 =
            FiniteTensor index2 $ (t1 *) <$> tensorData t2
        | otherwise = FiniteTensor index1 $ (* t2) <$> tensorData t1
        where
        -- Contraction of covariant and contravariant index
        FiniteTensor i1@(Covariant count1 _) ts1' `dot` FiniteTensor i2@(Contravariant count2 _) ts2'
            | count1 == count2 = sum $ (*) <$> ts1' <*> ts2'
            | otherwise = contractionErr i1 i2
        t1' `dot` t2' = contractionErr (tensorIndex t1') (tensorIndex t2')
        contractionErr i1' i2' = Err $
                "Tensor product: " ++ incompatibleTypes ++
                " - index1 is " ++ show i1' ++
                " and index2 is " ++ show i2'
    t1@(SimpleFinite index1 ts1) * t2@(SimpleFinite index2 ts2)
        | indexName index1 == indexName index2 = Scalar $ sum $ (*) <$> ts1 <*> ts2
        | indexName index1 `Data.List.elem` indicesNames t2 =
            FiniteTensor index2 $ (t1 *) <$> tensorData t2
        | otherwise = FiniteTensor index1 $ (* t2) <$> tensorData t1
        where
        -- Contraction of covariant and contravariant index
        SimpleFinite i1@(Covariant count1 _) ts1' `dot` SimpleFinite i2@(Contravariant count2 _) ts2'
            | count1 == count2 = Scalar $ sum $ (*) <$> ts1' <*> ts2'
            | otherwise = contractionErr i1 i2
        t1' `dot` t2' = contractionErr (tensorIndex t1') (tensorIndex t2')
        contractionErr i1' i2' = Err $
                "Tensor product: " ++ incompatibleTypes ++
                " - index1 is " ++ show i1' ++
                " and index2 is " ++ show i2'
    t1@(SimpleFinite index1 _) * t2@(FiniteTensor index2 _)
        | indexName index1 == indexName index2 = t1 `dot` t2
        | indexName index1 `Data.List.elem` indicesNames t2 =
            FiniteTensor index2 $ (t1 *) <$> tensorData t2
        | otherwise = FiniteTensor index1 $ (* t2) <$> tensorData t1
        where
        -- Contraction of covariant and contravariant index
        SimpleFinite i1@(Covariant count1 _) ts1' `dot` FiniteTensor i2@(Contravariant count2 _) ts2'
            | count1 == count2 = sum $ (.*) <$> ts1' <*> ts2'
            | otherwise = contractionErr i1 i2
        t1' `dot` t2' = contractionErr (tensorIndex t1') (tensorIndex t2')
        contractionErr i1' i2' = Err $
                "Tensor product: " ++ incompatibleTypes ++
                " - index1 is " ++ show i1' ++
                " and index2 is " ++ show i2'
    t1@(FiniteTensor index1 _) * t2@(SimpleFinite index2 _)
        | indexName index1 == indexName index2 = t1 `dot` t2
        | indexName index1 `Data.List.elem` indicesNames t2 =
            FiniteTensor index2 $ (t1 *) <$> tensorData t2
        | otherwise = FiniteTensor index1 $ (* t2) <$> tensorData t1
        where
        -- Contraction of covariant and contravariant index
        FiniteTensor i1@(Covariant count1 _) ts1' `dot` SimpleFinite i2@(Contravariant count2 _) ts2'
            | count1 == count2 = sum $ (*.) <$> ts1' <*> ts2'
            | otherwise = contractionErr i1 i2
        t1' `dot` t2' = contractionErr (tensorIndex t1') (tensorIndex t2')
        contractionErr i1' i2' = Err $
                "Tensor product: " ++ incompatibleTypes ++
                " - index1 is " ++ show i1' ++
                " and index2 is " ++ show i2'
    -- Multiplicating by error simply pushed this error forward
    Err msg * _ = Err msg
    _ * Err msg = Err msg -}
  
{-fmap f (FiniteTensor indexT (ZipList ts)) = case head ts of
        Scalar _         -> {-FiniteTensor indexT $ ZipList [Scalar (f x) | Scalar x <- ts]-} Err "FiniteTensor of Scalars! Merge to SimpleTensor!"
        FiniteTensor _ _ -> FiniteTensor indexT $ ZipList $ fmap (fmap f) ts
        Err msg          -> Err msg  -}