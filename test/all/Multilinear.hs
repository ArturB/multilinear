{-|
Module      : GPU
Description : Test of GPU tensor
Copyright   : (c) Artur M. Brodzki, 2018
License     : BSD3
Maintainer  : artur@brodzki.org
Stability   : experimental
Portability : Windows/POSIX

-}

module Multilinear (
    main
) where

import           Data.Maybe
import qualified Data.Set                       as Set
import           Multilinear.Class
import           Multilinear.Generic.GPU
import qualified Multilinear.Index              as Index
import           Test.QuickCheck
import           Test.QuickCheck.Common
import           Test.QuickCheck.Multilinear.Generic.GPU()

-- | Default test number for property
defTestN :: Int
defTestN = 200

----------------------------------------------------
-- TESTED TENSOR PROPERTIES FOR SEQUENTIAL TENSOR --
----------------------------------------------------


-- | Unary operator applied on any tensor,
-- | must preserve tensor indices in the result. 
preserveIndicesUnary :: Multilinear t a => 
   (t a -> 
    t a) -- ^ Unary tensor operator to test
 -> t a  -- ^ Operator argument
 -> Bool
preserveIndicesUnary f t = Set.fromList (indices t) == Set.fromList (indices (f t))

-- | Binary operator applied on any two tensors which have all the same indices, 
-- | must preserve set union of these indices in the result. 
preserveIndicesBinary :: Multilinear t a => 
   (t a -> 
    t a -> 
    t a) -- ^ Binary tensor operator to test
 -> t a  -- ^ First operator argument
 -> t a  -- ^ Second operator argument
 -> Bool
preserveIndicesBinary f t1 t2 = 
    let i1 = Set.fromList $ indices t1
        i2 = Set.fromList $ indices t2
    in  i1 /= i2 || i1 == Set.fromList (indices $ f t1 t2)

-- | Binary operator other than tensor product cannot contract (or consume) any index
-- | it means, that in operators other than (*), the indices of result tensor are set union of arguments indices
mergeCommonIndices :: Multilinear t a => 
   (t a -> 
    t a -> 
    t a) -- ^ Binary tensor operator to test
 -> t a  -- ^ First operator argument
 -> t a  -- ^ Second operator argument
 -> Bool
mergeCommonIndices f t1 t2 = 
    let inames1 = Set.fromList $ indicesNames t1
        inames2 = Set.fromList $ indicesNames t2
        commonIndicesNamesNoType = Set.intersection inames1 inames2
        expectedIndices = Set.union inames1 inames2
        resultIndices = Set.fromList $ Index.indexName <$> indices (f t1 t2)
        -- if we have indices, which have the same name but different type, it is forbidden and test passed
    in  Set.size commonIndicesNamesNoType /= Set.size (commonIndicesNames t1 t2) || 
        -- otherwise, the result indices set must be union of arguments indices
        expectedIndices == resultIndices
        
-- | Contracted indices have to be consumed in result tensor.
consumeContractedIndices :: Multilinear t a => 
    t a -- ^ first tensor to contract
 -> t a -- ^ second tensor to contract
 -> Bool
consumeContractedIndices t1 t2 = 
    let inames1 = Set.fromList $ Index.indexName <$> indices t1
        inames2 = Set.fromList $ Index.indexName <$> indices t2
        expectedIndices = Set.difference (Set.union inames1 inames2) (contractedIndices t1 t2)
        resultIndices = Set.fromList $ Index.indexName <$> indices (t1 * t2)
    in  expectedIndices == resultIndices

-- | Order of the tensor must be equal to number of its covariant and contravariant indices
orderIndices ::Multilinear t a => t a -> Bool
orderIndices t = 
    let (conv, cov) = order t 
        iConv = Set.fromList $ Index.isContravariant `Prelude.filter` indices t
        iCov  = Set.fromList $ Index.isCovariant `Prelude.filter` indices t
    in  conv == Set.size iConv && cov == Set.size iCov

-- | Tensor must be equivalent in terms of its indices after any index shift
shiftEquiv :: Multilinear t a => t a -> Bool
shiftEquiv t = 
    let inames = indicesNames t
        rShiftedTs = (\i -> t |>> i) <$> inames
        lShiftedTs = (\i -> t <<| i) <$> inames
        rtShiftedTs = (\i -> t |>>> i) <$> inames
        ltShiftedTs = (\i -> t <<<| i) <$> inames
        allShiftedTs = rShiftedTs ++ lShiftedTs ++ rtShiftedTs ++ ltShiftedTs ++ [t]
        allPairs = pure (,) <*> allShiftedTs <*> allShiftedTs
    in all (uncurry (|==|)) allPairs

{-| After rename, index must hold a new name
   This property assumes, tensor have max 5 indices of each type -}
renameTest :: Multilinear t a => t a -> Bool
renameTest t = 
    let (conv, cov) = order t
        convNs = take conv ['m' .. ]
        covNs  = take cov  ['s' .. ]
        renamedT = t $| (convNs, covNs)
        inamesAfter = concat $ indicesNames renamedT
    in  all (\i -> elem i convNs || elem i covNs) inamesAfter

-- | After any raising or lowering index, it must be a valid type. 
raiseLowerTest :: Multilinear t a => t a -> Bool
raiseLowerTest t = 
    let inames = indicesNames t
        lowered = inames `zip` ((t `lower`) <$> inames)
        raised = inames `zip` ((t `raise`) <$> inames)
        isLowered (i,tl) = i `elem` (Index.indexName <$> (Index.isCovariant     `Prelude.filter` indices tl))
        isRaised  (i,tr) = i `elem` (Index.indexName <$> (Index.isContravariant `Prelude.filter` indices tr))
    in  all isLowered lowered && all isRaised raised

transposeTest :: Multilinear t a => t a -> Bool
transposeTest t = 
    let indices1 = indices t
        inames1 = indicesNames t
        t2 = transpose t
        indices2 = indices t2
        inames2 = indicesNames t2
    in  isScalar t || ( inames1 == inames2 && indices1 /= indices2 )

-- | Filter second half of elements for each tensor index and check if they disappeared
filterIndexTest :: Multilinear t a => 
    t a -> Bool
filterIndexTest s@(Scalar _) = s == filterIndex "c" (Prelude.const True) s
filterIndexTest t = 
    let indsT = indices t
        -- filter second half of an index
        filteredHalf i = filterIndex (Index.indexName i) (< (fromJust (Index.indexSize i) `div` 2)) t
        fts = indsT `zip` (filteredHalf <$> indsT) -- tensors with filtered indices, paired with respective transformed indices
    in  all (\(i,ft) -> 
                size ft (Index.indexName i) == (fromJust (Index.indexSize i) `div` 2)
            ) fts

-- | Simple show test, just to check if function evaluates at all
showTest :: Show a => a -> Bool
showTest = not . null . show
