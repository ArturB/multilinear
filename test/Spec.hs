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

import qualified Data.Set                 as Set
import           Multilinear
import           Multilinear.Generic
import qualified Multilinear.Index        as Index
import           System.IO
import           Test.QuickCheck
import           Test.QuickCheck.Multilinear()

-- | Default test number for property
defTestN :: Int
defTestN = 1000

-- quickCheck with parametrizable tests number
quickCheckN :: Testable prop => Int -> prop -> IO ()
quickCheckN n = quickCheckWith (Args 
    Nothing -- ^ Should we replay a previous test? No. 
    n       -- ^ Maximum number of successful tests before succeeding set to N. 
    1       -- ^ Maximum number of discarded tests per successful test before giving up - gave up after first failure. 
    n       -- ^ Size to use for the biggest test cases.
    True    -- ^ Whether to print anything? yes. 
    0)      -- ^ Maximum number of shrinks to before giving up. Turn shrinking off.

-- | Print property test result
printPropertyTest :: (
    Testable prop 
    ) => String -- ^ Tested property name
      -> Int    -- ^ Number of tests to do
      -> prop   -- ^ Property to test
      -> IO ()
printPropertyTest propName n f = do
    putStr $ "  Checking " ++ propName ++ " "
    quickCheckN n f
    hFlush stdout

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

-- | Binary operator other than tensor product must merge common indices in result tensor
-- | it means, that in operators other than (*), the result tensor indices are set union of arguments indices
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

        iContravariantNames1 = Set.fromList $ Index.indexName <$> (Index.isContravariant `filter` indices t1)
        iCovariantNames1 = Set.fromList $ Index.indexName <$> (Index.isCovariant `filter` indices t1)

        iContravariantNames2 = Set.fromList $ Index.indexName <$> (Index.isContravariant `filter` indices t2)
        iCovariantNames2 = Set.fromList $ Index.indexName <$> (Index.isCovariant `filter` indices t2)

        contractedIndices = 
            -- contracted are indices covariant in the first tensor and contravariant in the second
            Set.intersection iCovariantNames1 iContravariantNames2 `Set.union`
            -- or contravariant in the first tensor and covariant in the second
            Set.intersection iContravariantNames1 iCovariantNames2
        
        expectedIndices = Set.difference (Set.union inames1 inames2) contractedIndices
        resultIndices = Set.fromList $ Index.indexName <$> indices (t1 * t2)

    in  expectedIndices == resultIndices

-- | Order of the tensor must be equal to number of its covariant and contravariant indices
orderIndices :: 
    Tensor Double
 -> Bool
orderIndices t = 
    let (conv, cov) = order t 
        iConv = Set.fromList $ Index.isContravariant `filter` indices t
        iCov  = Set.fromList $ Index.isCovariant `filter` indices t
    in  conv == Set.size iConv && cov == Set.size iCov

-- | Tensor must be equivalent in terms of its indices after any index shift
shiftEquiv :: 
    Tensor Double
 -> Bool
shiftEquiv t = 
    let inames = indicesNames t
        rShiftedTs = (\i -> t |>> i) <$> inames
        lShiftedTs = (\i -> t <<| i) <$> inames
        rtShiftedTs = (\i -> t |>>> i) <$> inames
        ltShiftedTs = (\i -> t <<<| i) <$> inames
        allShiftedTs = rShiftedTs ++ lShiftedTs ++ rtShiftedTs ++ ltShiftedTs ++ [t]
        allPairs = pure (,) <*> allShiftedTs <*> allShiftedTs
        allEquivs = uncurry (|==|) <$> allPairs
    in False `notElem` allEquivs

-- | After rename, index must hold a new name
-- | This property assumes, tensor have max 5 indices of each type
renameTest ::
    Tensor Double
 -> Bool
renameTest t = 
    let (conv, cov) = order t
        convNs = take conv ['m' .. ]
        covNs  = take cov  ['s' .. ]
        renamedT = t $| (convNs, covNs)
        inamesAfter = concat $ indicesNames renamedT
        inamesValid = (\i -> elem i convNs || elem i covNs) <$> inamesAfter
    in  False `notElem` inamesValid

-- | After any raising or lowering index, it must be a valid type
raiseLowerTest ::
    Tensor Double
 -> Bool
raiseLowerTest t = 
    let inames = indicesNames t
        lowered = inames `zip` ((t \/) <$> inames)
        raised = inames `zip` ((t /\) <$> inames)
        isLowered = (\(i,tl) -> i `elem` (Index.indexName <$> (Index.isCovariant     `filter` indices tl))) <$> lowered
        isRaised  = (\(i,tr) -> i `elem` (Index.indexName <$> (Index.isContravariant `filter` indices tr))) <$> raised
    in  False `notElem` isLowered ++ isRaised

filterLGTest :: 
    Int
 -> Tensor Double
 -> Bool
filterLGTest n t = 
    let tLess = (< n) `Multilinear.filter` t
        tGreat = (> n) `Multilinear.filter` t
    in  



-- | ENTRY POINT
main :: IO ()
main = do
    ---------------------------
    -- CHECKING NUM INSTANCE --
    ---------------------------

    printPropertyTest "preserveIndicesBinary for (+)"   defTestN $ preserveIndicesBinary (+)
    printPropertyTest "preserveIndicesBinary for (-)"   defTestN $ preserveIndicesBinary (-)
    printPropertyTest "preserveIndicesBinary for (*)"   defTestN $ preserveIndicesBinary (*)
    printPropertyTest "preserveIndicesUnary for abs"    defTestN $ preserveIndicesUnary abs
    printPropertyTest "preserveIndicesUnary for signum" defTestN $ preserveIndicesUnary signum

    printPropertyTest "mergeCommonIndices for (+)"      defTestN $ mergeCommonIndices (+)
    printPropertyTest "mergeCommonIndices for (-)"      defTestN $ mergeCommonIndices (-)
    printPropertyTest "consumeContractedIndices"        defTestN consumeContractedIndices
    
    --------------------------------
    -- CHECKING FLOATING INSTANCE --
    --------------------------------

    printPropertyTest "preserveIndicesUnary for exp"   defTestN $ preserveIndicesUnary exp
    printPropertyTest "preserveIndicesUnary for log"   defTestN $ preserveIndicesUnary log
    printPropertyTest "preserveIndicesUnary for sin"   defTestN $ preserveIndicesUnary sin
    printPropertyTest "preserveIndicesUnary for cos"   defTestN $ preserveIndicesUnary cos
    printPropertyTest "preserveIndicesUnary for asin"  defTestN $ preserveIndicesUnary asin
    printPropertyTest "preserveIndicesUnary for acos"  defTestN $ preserveIndicesUnary acos
    printPropertyTest "preserveIndicesUnary for atan"  defTestN $ preserveIndicesUnary atan
    printPropertyTest "preserveIndicesUnary for sinh"  defTestN $ preserveIndicesUnary sinh
    printPropertyTest "preserveIndicesUnary for cosh"  defTestN $ preserveIndicesUnary cosh
    printPropertyTest "preserveIndicesUnary for asinh" defTestN $ preserveIndicesUnary asinh
    printPropertyTest "preserveIndicesUnary for acosh" defTestN $ preserveIndicesUnary acosh
    printPropertyTest "preserveIndicesUnary for atanh" defTestN $ preserveIndicesUnary atanh

    -----------------------------------
    -- CHECKING MULTILINEAR INSTANCE --
    -----------------------------------

    printPropertyTest "preserveIndicesUnary for (+.)"   defTestN $ preserveIndicesUnary (5 +.)
    printPropertyTest "preserveIndicesUnary for (.+)"   defTestN $ preserveIndicesUnary (.+ 5)
    printPropertyTest "preserveIndicesUnary for (-.)"   defTestN $ preserveIndicesUnary (5 -.)
    printPropertyTest "preserveIndicesUnary for (.-)"   defTestN $ preserveIndicesUnary (.- 5)
    printPropertyTest "preserveIndicesUnary for (*.)"   defTestN $ preserveIndicesUnary (5 *.)
    printPropertyTest "preserveIndicesUnary for (.*)"   defTestN $ preserveIndicesUnary (.* 5)

    printPropertyTest "orderIndices" defTestN orderIndices
    printPropertyTest "shiftEquiv" defTestN shiftEquiv
    printPropertyTest "renamedTest" defTestN renameTest
    printPropertyTest "raiseLowerTest" defTestN raiseLowerTest

