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

import           Data.List
import qualified Data.Set                 as Set
import           Multilinear.Class
import           Multilinear.Generic
import qualified Multilinear.Index        as Index
import           System.IO
import           Test.QuickCheck
import           Test.QuickCheck.Multilinear()

-- quickCheck with parametrizable tests number
quickCheckN :: Testable prop => Int -> prop -> IO ()
quickCheckN n = quickCheckWith (Args 
    Nothing -- ^ Should we replay a previous test? No. 
    n       -- ^ Maximum number of successful tests before succeeding set to N. 
    1       -- ^ Maximum number of discarded tests per successful test before giving up - gave up after first failure. 
    n       -- ^ Size to use for the biggest test cases.
    True    -- ^ Whether to print anything? yes. 
    0)      -- ^ Maximum number of shrinks to before giving up. Turn shrinking off.

-- Print property test result
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

-- | Binary operator applied on any two tensors which have the same indices, 
-- | must preserve these indices in the result. 
preserveIndicesBinary ::
   (Tensor Double -> 
    Tensor Double -> 
    Tensor Double) -- ^ Binary tensor operator to test
 -> Tensor Double  -- ^ First operator argument
 -> Tensor Double  -- ^ Second operator argument
 -> Bool
preserveIndicesBinary f t1 t2 = 
    let i1 = indices t1
        i2 = indices t2
    in  i1 /= i2 || indices (f t1 t2) == i1

-- | Contracted indices have to be consumed in result tensor.
consumeContractedIndices :: 
    Tensor Double -- ^ first tensor to contract
 -> Tensor Double -- ^ second tensor to contract
 -> Bool
consumeContractedIndices t1 t2 = 
    let inames1 = Index.indexName <$> indices t1
        inames2 = Index.indexName <$> indices t2

        iContravariantNames1 = Index.indexName <$> (Index.isContravariant `filter` indices t1)
        iCovariantNames1 = Index.indexName <$> (Index.isCovariant `filter` indices t1)

        iContravariantNames2 = Index.indexName <$> (Index.isContravariant `filter` indices t2)
        iCovariantNames2 = Index.indexName <$> (Index.isCovariant `filter` indices t2)

        contractedIndices = 
            -- contracted are indices covariant in the first tensor and contravariant in the second
            (`Data.List.elem` iContravariantNames2) `filter` iCovariantNames1 ++ 
            -- or contravariant in the first tensor and covariant in the second
            (`Data.List.elem` iCovariantNames2) `filter` iContravariantNames1
        
        expectedIndices = (`Data.List.notElem` contractedIndices) `filter` (inames1 ++ inames2)
        expectedIndicesSet = Set.fromList expectedIndices
        resultIndicesSet = Set.fromList $ Index.indexName <$> indices (t1 * t2)
        
    in  expectedIndicesSet == resultIndicesSet


-- | ENTRY POINT
main :: IO ()
main = do
    ---------------------------
    -- CHECKING NUM INSTANCE --
    ---------------------------

    printPropertyTest "preserveIndicesBinary for (+)"   500 $ preserveIndicesBinary (+)
    printPropertyTest "preserveIndicesBinary for (-)"   500 $ preserveIndicesBinary (-)
    printPropertyTest "preserveIndicesBinary for (*)"   500 $ preserveIndicesBinary (*)
    printPropertyTest "preserveIndicesUnary for abs"    500 $ preserveIndicesUnary abs
    printPropertyTest "preserveIndicesUnary for signum" 500 $ preserveIndicesUnary signum
    printPropertyTest "consumeContractedIndices"        500 consumeContractedIndices
    
    --------------------------------
    -- CHECKING FLOATING INSTANCE --
    --------------------------------

    printPropertyTest "preserveIndicesUnary for exp"   500 $ preserveIndicesUnary exp
    printPropertyTest "preserveIndicesUnary for log"   500 $ preserveIndicesUnary log
    printPropertyTest "preserveIndicesUnary for sin"   500 $ preserveIndicesUnary sin
    printPropertyTest "preserveIndicesUnary for cos"   500 $ preserveIndicesUnary cos
    printPropertyTest "preserveIndicesUnary for asin"  500 $ preserveIndicesUnary asin
    printPropertyTest "preserveIndicesUnary for acos"  500 $ preserveIndicesUnary acos
    printPropertyTest "preserveIndicesUnary for atan"  500 $ preserveIndicesUnary atan
    printPropertyTest "preserveIndicesUnary for sinh"  500 $ preserveIndicesUnary sinh
    printPropertyTest "preserveIndicesUnary for cosh"  500 $ preserveIndicesUnary cosh
    printPropertyTest "preserveIndicesUnary for asinh" 500 $ preserveIndicesUnary asinh
    printPropertyTest "preserveIndicesUnary for acosh" 500 $ preserveIndicesUnary acosh
    printPropertyTest "preserveIndicesUnary for atanh" 500 $ preserveIndicesUnary atanh

    -----------------------------------
    -- CHECKING MULTILINEAR INSTANCE --
    -----------------------------------



