
-- dot product of two tensors
dot' :: (
    Num a, Bits a
    ) => Tensor a                             -- ^ First dot product argument
      -> Tensor a                             -- ^ Second dot product argument
      -> Tensor a                             -- ^ Resulting dot product

-- Two simple tensors product
dot' (SimpleFinite i1@(Finite.Covariant count1 _) ts1') (SimpleFinite i2@(Finite.Contravariant count2 _) ts2')
    | count1 == count2 =
        --let dotProduct v1 v2 = Boxed.sum $ Boxed.zipWith (*) v1 v2
        Scalar $ Boxed.sum $ Boxed.zipWith (*) ts1' ts2'
    | otherwise = contractionErr (Index.toTIndex i1) (Index.toTIndex i2)

-- Two finite tensors product
dot' (FiniteTensor i1@(Finite.Covariant count1 _) ts1') (FiniteTensor i2@(Finite.Contravariant count2 _) ts2')
    | count1 == count2 = Boxed.sum $ Boxed.zipWith (*) ts1' ts2'
    | otherwise = contractionErr (Index.toTIndex i1) (Index.toTIndex i2)

-- Simple tensor and finite tensor product
dot' (SimpleFinite i1@(Finite.Covariant count1 _) ts1') (FiniteTensor i2@(Finite.Contravariant count2 _) ts2')
    | count1 == count2 = Boxed.sum $ Boxed.zipWith (.*) ts1' ts2'
    | otherwise = contractionErr (Index.toTIndex i1) (Index.toTIndex i2)

-- Finite tensor and simple tensor product
dot' (FiniteTensor i1@(Finite.Covariant count1 _) ts1') (SimpleFinite i2@(Finite.Contravariant count2 _) ts2')
    | count1 == count2 = Boxed.sum $ Boxed.zipWith (*.) ts1' ts2'
    | otherwise = contractionErr (Index.toTIndex i1) (Index.toTIndex i2)

-- Simple tensor and infinite tensor product
dot' (SimpleFinite (Finite.Covariant count1 _) ts1') (InfiniteTensor (Infinite.Contravariant _) ts2') =
    Boxed.sum $ Boxed.zipWith (.*) ts1' (Boxed.fromList $ Prelude.take count1 ts2')

-- Infinite tensor and simple tensor product
dot' (InfiniteTensor (Infinite.Covariant _) ts1') (SimpleFinite (Finite.Contravariant count2 _) ts2') =
    Boxed.sum $ Boxed.zipWith (*.) (Boxed.fromList $ Prelude.take count2 ts1') ts2'

-- Finite tensor and infinite tensor product
dot' (FiniteTensor (Finite.Covariant count1 _) ts1') (InfiniteTensor (Infinite.Contravariant _) ts2') =
    Boxed.sum $ Boxed.zipWith (*) ts1' (Boxed.fromList $ Prelude.take count1 ts2')

-- Infinite tensor and finite tensor product
dot' (InfiniteTensor (Infinite.Covariant _) ts1') (FiniteTensor (Finite.Contravariant count2 _) ts2') =
    Boxed.sum $ Boxed.zipWith (*) (Boxed.fromList $ Prelude.take count2 ts1') ts2'

-- In other cases cannot happen!
dot' t1' t2' = contractionErr (tensorIndex t1') (tensorIndex t2')

-----------------------------------------------------------------------------


-- Two simple tensors product
dot (SimpleFinite i1@(Finite.Covariant count1 _) ts1') (SimpleFinite i2@(Finite.Contravariant count2 _) ts2')
    | count1 == count2 = 
        --let dotProduct v1 v2 = Boxed.sum $ Boxed.zipWith (*) v1 v2
        Scalar $ Boxed.sum $ Boxed.zipWith (*) ts1' ts2'
    | otherwise = contractionErr (Index.toTIndex i1) (Index.toTIndex i2)

-- Two finite tensors product
dot (FiniteTensor i1@(Finite.Covariant count1 _) ts1') (FiniteTensor i2@(Finite.Contravariant count2 _) ts2')
    | count1 == count2 = Boxed.sum $ Boxed.zipWith (*) ts1' ts2'
    | otherwise = contractionErr (Index.toTIndex i1) (Index.toTIndex i2)

-- Simple tensor and finite tensor product
dot (SimpleFinite i1@(Finite.Covariant count1 _) ts1') (FiniteTensor i2@(Finite.Contravariant count2 _) ts2')
    | count1 == count2 = Boxed.sum $ Boxed.zipWith (.*) ts1' ts2'
    | otherwise = contractionErr (Index.toTIndex i1) (Index.toTIndex i2)

-- Finite tensor and simple tensor product
dot (FiniteTensor i1@(Finite.Covariant count1 _) ts1') (SimpleFinite i2@(Finite.Contravariant count2 _) ts2')
    | count1 == count2 = Boxed.sum $ Boxed.zipWith (*.) ts1' ts2'
    | otherwise = contractionErr (Index.toTIndex i1) (Index.toTIndex i2)

-- Simple tensor and infinite tensor product
dot (SimpleFinite (Finite.Covariant count1 _) ts1') (InfiniteTensor (Infinite.Contravariant _) ts2') = 
    Boxed.sum $ Boxed.zipWith (.*) ts1' (Boxed.fromList $ take count1 ts2')

-- Infinite tensor and simple tensor product
dot (InfiniteTensor (Infinite.Covariant _) ts1') (SimpleFinite (Finite.Contravariant count2 _) ts2') = 
    Boxed.sum $ Boxed.zipWith (*.) (Boxed.fromList $ take count2 ts1') ts2'

-- Finite tensor and infinite tensor product
dot (FiniteTensor (Finite.Covariant count1 _) ts1') (InfiniteTensor (Infinite.Contravariant _) ts2') = 
    Boxed.sum $ Boxed.zipWith (*) ts1' (Boxed.fromList $ take count1 ts2')

-- Infinite tensor and finite tensor product
dot (InfiniteTensor (Infinite.Covariant _) ts1') (FiniteTensor (Finite.Contravariant count2 _) ts2') = 
    Boxed.sum $ Boxed.zipWith (*) (Boxed.fromList $ take count2 ts1') ts2'

-- In other cases cannot happen!
dot t1' t2' = contractionErr (tensorIndex t1') (tensorIndex t2')
