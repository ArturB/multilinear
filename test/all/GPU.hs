{-|
Module      : GPU
Description : Test of GPU tensor
Copyright   : (c) Artur M. Brodzki, 2018
License     : BSD3
Maintainer  : artur@brodzki.org
Stability   : experimental
Portability : Windows/POSIX

-}

module GPU (
    main
) where

import           Multilinear.Class
import           Multilinear.Generic.GPU
import qualified ConstructorsTests
import qualified MultilinearTests
import           Test.QuickCheck
import           Test.QuickCheck.Common
import           Test.QuickCheck.Multilinear.Generic.GPU()

-- | Default test number for property
defTestN :: Int
defTestN = 200

---------------------------------------------------
-- TYPE-SPECIALIED VERSIONS OF TESTING FUNCTIONS --
---------------------------------------------------

preserveIndicesUnary :: 
   (Tensor Double -> 
    Tensor Double) -- ^ Unary tensor operator to test
 -> Tensor Double  -- ^ Operator argument
 -> Bool
preserveIndicesUnary = MultilinearTests.preserveIndicesUnary

preserveIndicesBinary ::
   (Tensor Double -> 
    Tensor Double -> 
        Tensor Double) -- ^ Binary tensor operator to test
 -> Tensor Double      -- ^ First operator argument
 -> Tensor Double      -- ^ Second operator argument
 -> Bool
preserveIndicesBinary = MultilinearTests.preserveIndicesBinary

mergeCommonIndices ::
   (Tensor Double -> 
    Tensor Double -> 
    Tensor Double) -- ^ Binary tensor operator to test
 -> Tensor Double  -- ^ First operator argument
 -> Tensor Double  -- ^ Second operator argument
 -> Bool
mergeCommonIndices = MultilinearTests.mergeCommonIndices
        
consumeContractedIndices :: 
    Tensor Double -- ^ first tensor to contract
 -> Tensor Double -- ^ second tensor to contract
 -> Bool
consumeContractedIndices = MultilinearTests.consumeContractedIndices

orderIndices :: Tensor Double -> Bool
orderIndices = MultilinearTests.orderIndices

shiftEquiv :: Tensor Double -> Bool
shiftEquiv = MultilinearTests.shiftEquiv

renameTest :: Tensor Double -> Bool
renameTest = MultilinearTests.renameTest

raiseLowerTest :: Tensor Double -> Bool
raiseLowerTest = MultilinearTests.raiseLowerTest

transposeTest :: Tensor Double -> Bool
transposeTest = MultilinearTests.transposeTest

filterIndexTest :: Tensor Double -> Bool
filterIndexTest = MultilinearTests.filterIndexTest

showTest :: Tensor Double -> Bool
showTest = MultilinearTests.showTest

vectorConstructorValues :: Char -> Positive (Small Int) -> Tensor Double -> Bool
vectorConstructorValues = ConstructorsTests.vectorConstructorValues

formConstructorValues :: Char -> Positive (Small Int) -> Tensor Double -> Bool
formConstructorValues = ConstructorsTests.formConstructorValues

matrixConstructorValues :: Char -> Char -> Positive (Small Int) -> Positive (Small Int) -> Tensor Double -> Bool
matrixConstructorValues = ConstructorsTests.matrixConstructorValues

nVectorConstructorValues :: Char -> Char -> Positive (Small Int) -> Positive (Small Int) -> Tensor Double -> Bool
nVectorConstructorValues = ConstructorsTests.nVectorConstructorValues

nFormConstructorValues :: Char -> Char -> Positive (Small Int) -> Positive (Small Int) -> Tensor Double -> Bool
nFormConstructorValues = ConstructorsTests.nFormConstructorValues

vectorConstructorError :: Char -> Positive (Small Int) -> Tensor Double -> Property
vectorConstructorError = ConstructorsTests.vectorConstructorError

formConstructorError :: Char -> Positive (Small Int) -> Tensor Double -> Property
formConstructorError = ConstructorsTests.formConstructorError

matrixConstructorError :: Char -> Char -> Positive (Small Int) -> Positive (Small Int) -> Tensor Double -> Property
matrixConstructorError = ConstructorsTests.matrixConstructorError

nVectorConstructorError :: Char -> Char -> Positive (Small Int) -> Positive (Small Int) -> Tensor Double -> Property
nVectorConstructorError = ConstructorsTests.nVectorConstructorError

nFormConstructorError :: Char -> Char -> Positive (Small Int) -> Positive (Small Int) -> Tensor Double -> Property
nFormConstructorError = ConstructorsTests.nFormConstructorError

-- | Check indices preservation if zipWith function
zipWithTest :: Tensor Double -> Tensor Double -> Bool
zipWithTest t1@(Scalar _) t2 = preserveIndicesUnary (\t -> Multilinear.Generic.GPU.zipWith (+) t1 t) t2
zipWithTest t1 t2@(Scalar _) = preserveIndicesUnary (\t -> Multilinear.Generic.GPU.zipWith (+) t t2) t1
zipWithTest t1 _ = preserveIndicesBinary (Multilinear.Generic.GPU.zipWith (+)) t1 t1

-- | ENTRY POINT
main :: IO ()
main = do

    -- PRINT PROBABILITY DISTRIBUTION OF TESTED TENSORS ORDER
    executePropertyTest "probability distribution of tensors order" 10000 $ 
        \(t :: Tensor Double) -> collect (order t) $ preserveIndicesUnary abs
    executePropertyTest "probability distribution of contracted indices" 10000 $
        \(t1 :: Tensor Double, t2 :: Tensor Double) -> collect (length $ contractedIndices t1 t2) $ preserveIndicesBinary (+)

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

    ----------------------------------
    -- CHECKING FRACTIONAL INSTANCE --
    ----------------------------------

    -- TODO
    
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

    executePropertyTest "orderIndices" defTestN orderIndices
    executePropertyTest "shiftEquiv" defTestN shiftEquiv
    executePropertyTest "renamedTest" defTestN renameTest
    executePropertyTest "raiseLowerTest" defTestN raiseLowerTest
    executePropertyTest "transposeTest" defTestN transposeTest

    -----------------------------------
    -- CHECKING AUXILIRARY FUNCTIONS --
    -----------------------------------

    executePropertyTest "preserveIndicesUnary for (+.)"   defTestN $ preserveIndicesUnary (5 +.)
    executePropertyTest "preserveIndicesUnary for (.+)"   defTestN $ preserveIndicesUnary (.+ 5)
    executePropertyTest "preserveIndicesUnary for (-.)"   defTestN $ preserveIndicesUnary (5 -.)
    executePropertyTest "preserveIndicesUnary for (.-)"   defTestN $ preserveIndicesUnary (.- 5)
    executePropertyTest "preserveIndicesUnary for (*.)"   defTestN $ preserveIndicesUnary (5 *.)
    executePropertyTest "preserveIndicesUnary for (.*)"   defTestN $ preserveIndicesUnary (.* 5)
    executePropertyTest "filterIndexTest" defTestN filterIndexTest
    executePropertyTest "zipWithTest" defTestN zipWithTest
    executePropertyTest "showTest" 100 showTest

    -----------------------------------
    -- CHECKING GENERIC CONSTRUCTORS --
    -----------------------------------

    executePropertyTest "vectorConstructorError"  defTestN vectorConstructorError
    executePropertyTest "formConstructorError"    defTestN formConstructorError
    executePropertyTest "matrixConstructorError"  defTestN matrixConstructorError
    executePropertyTest "nFormConstructorError"   defTestN nFormConstructorError
    executePropertyTest "nVectorConstructorError" defTestN nVectorConstructorError

    executePropertyTest "vectorContructorValues"   100 vectorConstructorValues
    executePropertyTest "formContructorValues"     100 formConstructorValues
    executePropertyTest "matrixConstructorValues"  100 matrixConstructorValues
    executePropertyTest "nFormConstructorValues"   100 nFormConstructorValues
    executePropertyTest "nVectorConstructorValues" 100 nVectorConstructorValues
