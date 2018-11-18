{-|
Module      : Main
Description : Test of Multilinear library
Copyright   : (c) Artur M. Brodzki, 2018
License     : BSD3
Maintainer  : artur@brodzki.org
Stability   : experimental
Portability : Windows/POSIX

-}

module Main (
    main
) where

import           Data.Maybe
import qualified Data.Set                 as Set
import           Multilinear
import           Multilinear.Generic
import qualified Multilinear.Index        as Index
import           System.Exit
import           System.IO
import           Test.QuickCheck
import           Test.QuickCheck.Multilinear()

-- | Default test number for property
defTestN :: Int
defTestN = 1000


------------------------------
-- AUXILIARY TEST FUNCTIONS --
------------------------------


-- quickCheck with parametrizable tests number
quickCheckN :: Testable prop => Int -> prop -> IO Result
quickCheckN n = quickCheckWithResult (Args 
    Nothing -- ^ Should we replay a previous test? No. 
    n       -- ^ Maximum number of successful tests before succeeding set to N. 
    1       -- ^ Maximum number of discarded tests per successful test before giving up - gave up after first failure. 
    n       -- ^ Size to use for the biggest test cases.
    True    -- ^ Whether to print anything? yes. 
    0)      -- ^ Maximum number of shrinks to before giving up. Turn shrinking off.

-- | Execute property test and check result:
-- | exit test suite with successs code if no errors occured
-- | exit test suite with failure code if any error occured
executePropertyTest :: (
    Testable prop 
    ) => String -- ^ Tested property name
      -> Int    -- ^ Number of tests to do
      -> prop   -- ^ Property to test
      -> IO ()
executePropertyTest propName n f = do
    putStr $ "  Checking " ++ propName ++ " "
    r <- quickCheckN n f
    case r of
        Success _ _ _  -> hFlush stdout
        _ -> exitFailure


------------------------------
-- TESTED TENSOR PROPERTIES --
------------------------------


-- | Unary operator applied on any tensor,
-- | must preserve tensor indices in the result. 
preserveIndicesUnary ::
   (Tensor Double -> 
    Tensor Double) -- ^ Unary tensor operator to test
 -> Tensor Double  -- ^ Operator argument
 -> Bool
preserveIndicesUnary f t = indices t == indices (f t)

-- | Binary operator applied on any two tensors which have all the same indices, 
-- | must preserve set union of these indices in the result. 
preserveIndicesBinary ::
   (Tensor Double -> 
    Tensor Double -> 
    Tensor Double) -- ^ Binary tensor operator to test
 -> Tensor Double  -- ^ First operator argument
 -> Tensor Double  -- ^ Second operator argument
 -> Bool
preserveIndicesBinary f t1 t2 = 
    let i1 = Set.fromList $ indices t1
        i2 = Set.fromList $ indices t2
    in  i1 /= i2 || i1 == Set.fromList (indices $ f t1 t2)

-- | Binary operator other than tensor product cannot contract (or consume) any index
-- | it means, that in operators other than (*), the indices of result tensor are set union of arguments indices
mergeCommonIndices :: 
   (Tensor Double -> 
    Tensor Double -> 
    Tensor Double) -- ^ Binary tensor operator to test
 -> Tensor Double  -- ^ First operator argument
 -> Tensor Double  -- ^ Second operator argument
 -> Bool
mergeCommonIndices f t1 t2 = 
    let indices1 = Set.fromList $ indices t1
        indices2 = Set.fromList $ indices t2
        inames1 = Set.fromList $ Index.indexName <$> indices t1
        inames2 = Set.fromList $ Index.indexName <$> indices t2

        commonIndices = Set.intersection indices1 indices2
        commonIndicesNames = Set.intersection inames1 inames2
        
        expectedIndices = Set.union inames1 inames2
        resultIndices = Set.fromList $ Index.indexName <$> indices (f t1 t2)

        -- if we have indices, which have the same name but different type, it is forbidden and test passed
    in  Set.size commonIndices /= Set.size commonIndicesNames || 
        -- otherwise, the result indices set must be union of arguments indices
        expectedIndices == resultIndices


-- | Contracted indices have to be consumed in result tensor.
consumeContractedIndices :: 
    Tensor Double -- ^ first tensor to contract
 -> Tensor Double -- ^ second tensor to contract
 -> Bool
consumeContractedIndices t1 t2 = 
    let inames1 = Set.fromList $ Index.indexName <$> indices t1
        inames2 = Set.fromList $ Index.indexName <$> indices t2

        iContravariantNames1 = Set.fromList $ Index.indexName <$> (Index.isContravariant `Prelude.filter` indices t1)
        iCovariantNames1 = Set.fromList $ Index.indexName <$> (Index.isCovariant `Prelude.filter` indices t1)

        iContravariantNames2 = Set.fromList $ Index.indexName <$> (Index.isContravariant `Prelude.filter` indices t2)
        iCovariantNames2 = Set.fromList $ Index.indexName <$> (Index.isCovariant `Prelude.filter` indices t2)

        contractedIndices = 
            -- contracted are indices covariant in the first tensor and contravariant in the second
            Set.intersection iCovariantNames1 iContravariantNames2 `Set.union`
            -- or contravariant in the first tensor and covariant in the second
            Set.intersection iContravariantNames1 iCovariantNames2
        
        expectedIndices = Set.difference (Set.union inames1 inames2) contractedIndices
        resultIndices = Set.fromList $ Index.indexName <$> indices (t1 * t2)

    in  expectedIndices == resultIndices

-- | Order of the tensor must be equal to number of its covariant and contravariant indices
orderIndices :: Tensor Double -> Bool
orderIndices t = 
    let (conv, cov) = order t 
        iConv = Set.fromList $ Index.isContravariant `Prelude.filter` indices t
        iCov  = Set.fromList $ Index.isCovariant `Prelude.filter` indices t
    in  conv == Set.size iConv && cov == Set.size iCov

-- | Tensor must be equivalent in terms of its indices after any index shift
shiftEquiv :: Tensor Double -> Bool
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
renameTest :: Tensor Double -> Bool
renameTest t = 
    let (conv, cov) = order t
        convNs = take conv ['m' .. ]
        covNs  = take cov  ['s' .. ]
        renamedT = t $| (convNs, covNs)
        inamesAfter = concat $ indicesNames renamedT
    in  all (\i -> elem i convNs || elem i covNs) inamesAfter

-- | After any raising or lowering index, it must be a valid type. 
raiseLowerTest :: Tensor Double -> Bool
raiseLowerTest t = 
    let inames = indicesNames t
        lowered = inames `zip` ((t \/) <$> inames)
        raised = inames `zip` ((t /\) <$> inames)
        isLowered (i,tl) = i `elem` (Index.indexName <$> (Index.isCovariant     `Prelude.filter` indices tl))
        isRaised  (i,tr) = i `elem` (Index.indexName <$> (Index.isContravariant `Prelude.filter` indices tr))
    in  all isLowered lowered && all isRaised raised


-- | Filter second half of elements for each tensor index and check if they disappeared
filterIndexTest :: 
    Tensor Double -> Bool
filterIndexTest s@(Scalar _) = s == filterIndex "c" (const True) s
filterIndexTest t = 
    let indsT = indices t
        -- filter second half of an index
        filteredHalf i = filterIndex (Index.indexName i) (< (fromJust (Index.indexSize i) `div` 2)) t
        fts = indsT `zip` (filteredHalf <$> indsT) -- tensors with filtered indices, paired with respective transformed indices
    in  all (\(i,ft) -> 
                size ft (Index.indexName i) == (fromJust (Index.indexSize i) `div` 2)
            ) fts

-- | ENTRY POINT
main :: IO ()
main = do

    -- PRINT PROBABILITY DISTRIBUTION OF TESTED TENSORS ORDER
    executePropertyTest "probability distribution of order of tested tensors" 5000 $ 
        \(t :: Tensor Double) -> collect (order t) $ preserveIndicesUnary abs

    putStrLn "\nTesting multilinear library...\n"

    ---------------------------
    -- CHECKING NUM INSTANCE --
    ---------------------------

    executePropertyTest "preserveIndicesBinary for (+)"   defTestN $ preserveIndicesBinary (+)
    executePropertyTest "preserveIndicesBinary for (-)"   defTestN $ preserveIndicesBinary (-)
    executePropertyTest "preserveIndicesBinary for (*)"   defTestN $ preserveIndicesBinary (*)
    executePropertyTest "preserveIndicesUnary for abs"    defTestN $ preserveIndicesUnary abs
    executePropertyTest "preserveIndicesUnary for signum" defTestN $ preserveIndicesUnary signum

    executePropertyTest "mergeCommonIndices for (+)"      defTestN $ mergeCommonIndices (+)
    executePropertyTest "mergeCommonIndices for (-)"      defTestN $ mergeCommonIndices (-)
    executePropertyTest "consumeContractedIndices"        defTestN consumeContractedIndices
    
    --------------------------------
    -- CHECKING FLOATING INSTANCE --
    --------------------------------

    executePropertyTest "preserveIndicesUnary for exp"   defTestN $ preserveIndicesUnary exp
    executePropertyTest "preserveIndicesUnary for log"   defTestN $ preserveIndicesUnary log
    executePropertyTest "preserveIndicesUnary for sin"   defTestN $ preserveIndicesUnary sin
    executePropertyTest "preserveIndicesUnary for cos"   defTestN $ preserveIndicesUnary cos
    executePropertyTest "preserveIndicesUnary for asin"  defTestN $ preserveIndicesUnary asin
    executePropertyTest "preserveIndicesUnary for acos"  defTestN $ preserveIndicesUnary acos
    executePropertyTest "preserveIndicesUnary for atan"  defTestN $ preserveIndicesUnary atan
    executePropertyTest "preserveIndicesUnary for sinh"  defTestN $ preserveIndicesUnary sinh
    executePropertyTest "preserveIndicesUnary for cosh"  defTestN $ preserveIndicesUnary cosh
    executePropertyTest "preserveIndicesUnary for asinh" defTestN $ preserveIndicesUnary asinh
    executePropertyTest "preserveIndicesUnary for acosh" defTestN $ preserveIndicesUnary acosh
    executePropertyTest "preserveIndicesUnary for atanh" defTestN $ preserveIndicesUnary atanh

    -----------------------------------
    -- CHECKING MULTILINEAR INSTANCE --
    -----------------------------------

    executePropertyTest "preserveIndicesUnary for (+.)"   defTestN $ preserveIndicesUnary (5 +.)
    executePropertyTest "preserveIndicesUnary for (.+)"   defTestN $ preserveIndicesUnary (.+ 5)
    executePropertyTest "preserveIndicesUnary for (-.)"   defTestN $ preserveIndicesUnary (5 -.)
    executePropertyTest "preserveIndicesUnary for (.-)"   defTestN $ preserveIndicesUnary (.- 5)
    executePropertyTest "preserveIndicesUnary for (*.)"   defTestN $ preserveIndicesUnary (5 *.)
    executePropertyTest "preserveIndicesUnary for (.*)"   defTestN $ preserveIndicesUnary (.* 5)

    executePropertyTest "orderIndices" defTestN orderIndices
    executePropertyTest "shiftEquiv" defTestN shiftEquiv
    executePropertyTest "renamedTest" defTestN renameTest
    executePropertyTest "raiseLowerTest" defTestN raiseLowerTest
    executePropertyTest "filterIndexTest" defTestN filterIndexTest
    executePropertyTest "zipWithIndicesTest" defTestN $ preserveIndicesUnary (\t -> Multilinear.zipWith (+) t t)

