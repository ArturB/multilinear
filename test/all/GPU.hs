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
import qualified Multilinear.Form               as Form
import qualified Multilinear.Matrix             as Matrix
import qualified Multilinear.Vector             as Vector
import qualified Multilinear.NForm              as NForm
import qualified Multilinear.NVector            as NVector
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





-- | Test generic vector constructor indices
vectorConstructor :: Char -> Positive (Small Int) -> Bool
vectorConstructor c s = 
    let size = getSmall $ getPositive s
        v :: Tensor Double = Vector.fromIndices [c] size fromIntegral
        vConst :: Tensor Double = Vector.const [c] size (fromIntegral size)
    in  v `Multilinear.Class.size` [c] == size && vConst `Multilinear.Class.size` [c] == size

-- | Test generic form constructor indices
formConstructor :: Char -> Positive (Small Int) -> Bool
formConstructor c s = 
    let size = getSmall $ getPositive s
        f :: Tensor Double = Form.fromIndices [c] size fromIntegral
        fConst :: Tensor Double = Form.const [c] size (fromIntegral size)
    in  f `Multilinear.Class.size` [c] == size && fConst `Multilinear.Class.size` [c] == size

-- | Test generic matrix constructor indices
matrixConstructor :: Char -> Char -> Positive (Small Int) -> Positive (Small Int) -> Bool
matrixConstructor c1 c2 s1 s2 = 
    let size1 = getSmall $ getPositive s1
        size2 = getSmall $ getPositive s2
        v :: Tensor Double = Matrix.fromIndices [c1,c2] size1 size2 (\x y -> fromIntegral x + fromIntegral y)
        vConst :: Tensor Double = Matrix.const [c1,c2] size1 size2 (fromIntegral size1)
    in  c1 == c2 || ( v `Multilinear.Class.size` [c1] == size1 && v `Multilinear.Class.size` [c2] == size2 
              && vConst `Multilinear.Class.size` [c1] == size1 && vConst `Multilinear.Class.size` [c2] == size2 )

-- | Test generic NVector constructor indices
nVectorConstructor :: Char -> Char -> Positive (Small Int) -> Positive (Small Int) -> Bool
nVectorConstructor c1 c2 s1 s2 = 
    let size1 = getSmall $ getPositive s1
        size2 = getSmall $ getPositive s2
        v :: Tensor Double = NVector.fromIndices [c1,c2] [size1,size2] (\[x,y] -> fromIntegral x + fromIntegral y)
        vConst :: Tensor Double = NVector.const [c1,c2] [size1,size2] (fromIntegral size1)
    in  c1 == c2 || ( v `Multilinear.Class.size` [c1] == size1 && v `Multilinear.Class.size` [c2] == size2
              && vConst `Multilinear.Class.size` [c1] == size1 && vConst `Multilinear.Class.size` [c2] == size2 )

-- | Test generic NForm constructor indices
nFormConstructor :: Char -> Char -> Positive (Small Int) -> Positive (Small Int) -> Bool
nFormConstructor c1 c2 s1 s2 = 
    let size1 = getSmall $ getPositive s1
        size2 = getSmall $ getPositive s2
        v :: Tensor Double = NForm.fromIndices [c1,c2] [size1,size2] (\[x,y] -> fromIntegral x + fromIntegral y)
        vConst :: Tensor Double = NForm.const [c1,c2] [size1,size2] (fromIntegral size1)
    in  c1 == c2 || ( v `Multilinear.Class.size` [c1] == size1 && v `Multilinear.Class.size` [c2] == size2
              && vConst `Multilinear.Class.size` [c1] == size1 && vConst `Multilinear.Class.size` [c2] == size2 )

-- | Test generic vector constructor indices error
vectorConstructorError :: Char -> Positive (Small Int) -> Property
vectorConstructorError c s = 
    let size = getSmall $ getPositive s
        v :: Tensor Double = Vector.fromIndices [c,'a'] size fromIntegral
    in  expectFailure (total v)

-- | Test generic form constructor indices error
formConstructorError :: Char -> Positive (Small Int) -> Property
formConstructorError c s = 
    let size = getSmall $ getPositive s
        f :: Tensor Double = Form.fromIndices [c,'a'] size fromIntegral
    in  expectFailure (total f)

-- | Test generic matrix constructor indices error
matrixConstructorError :: Char -> Char -> Positive (Small Int) -> Positive (Small Int) -> Property
matrixConstructorError c1 c2 s1 s2 = 
    let size1 = getSmall $ getPositive s1
        size2 = getSmall $ getPositive s2
        v :: Tensor Double = Matrix.fromIndices [c1,c2,'a'] size1 size2 (\x y -> fromIntegral x + fromIntegral y)
    in  expectFailure (total v)

-- | Test generic NVector constructor indices error
nVectorConstructorError :: Char -> Char -> Positive (Small Int) -> Positive (Small Int) -> Property
nVectorConstructorError c1 c2 s1 s2 = 
    let size1 = getSmall $ getPositive s1
        size2 = getSmall $ getPositive s2
        v :: Tensor Double = NVector.fromIndices [c1,c2,'a'] [size1,size2] (\[x,y] -> fromIntegral x + fromIntegral y)
    in  expectFailure (total v)

-- | Test generic NForm constructor indices error
nFormConstructorError :: Char -> Char -> Positive (Small Int) -> Positive (Small Int) -> Property
nFormConstructorError c1 c2 s1 s2 = 
    let size1 = getSmall $ getPositive s1
        size2 = getSmall $ getPositive s2
        v :: Tensor Double = NForm.fromIndices [c1,c2,'a'] [size1,size2] (\[x,y] -> fromIntegral x + fromIntegral y)
    in  expectFailure (total v)


-- | Test generic vector constructor values
vectorConstructorValues :: Char -> Positive (Small Int) -> Bool
vectorConstructorValues c s = 
    let size = getSmall $ getPositive s
        v :: Tensor Double = Vector.fromIndices [c] size fromIntegral
        vConst :: Tensor Double = Vector.const [c] size (fromIntegral size)
    in  all (\i -> v $$| ([c],[i]) == fromIntegral i) [0 .. size - 1] && 
        all (\i -> vConst $$| ([c],[i]) == fromIntegral size) [0 .. size - 1]

-- | Test generic form constructor values
formConstructorValues :: Char -> Positive (Small Int) -> Bool
formConstructorValues c s = 
    let size = getSmall $ getPositive s
        f :: Tensor Double = Form.fromIndices [c] size fromIntegral
        fConst :: Tensor Double = Form.const [c] size (fromIntegral size)
    in  all (\i -> f $$| ([c],[i]) == fromIntegral i) [0.. size - 1] && 
        all (\i -> fConst $$| ([c],[i]) == fromIntegral size) [0 .. size - 1]

-- | Test generic matrix constructor values
matrixConstructorValues :: Char -> Char -> Positive (Small Int) -> Positive (Small Int) -> Bool
matrixConstructorValues c1 c2 s1 s2 = 
    let size1 = getSmall $ getPositive s1
        size2 = getSmall $ getPositive s2
        v :: Tensor Double = Matrix.fromIndices [c1,c2] size1 size2 (\x y -> fromIntegral x + fromIntegral y)
        vConst :: Tensor Double = Matrix.const [c1,c2] size1 size2 (fromIntegral size1)
    in  c1 == c2 || (
        all (\(i1,i2) -> v $$| ([c1,c2],[i1,i2]) == fromIntegral i1 + fromIntegral i2) 
            (pure (,) <*> [0 .. size1 - 1] <*> [0 .. size2 - 1]) && 
        all (\(i1,i2) -> vConst $$| ([c1,c2],[i1,i2]) == fromIntegral size1) 
            (pure (,) <*> [0 .. size1 - 1] <*> [0 .. size2 - 1])
        )

-- | Test generic NVector constructor values
nVectorConstructorValues :: Char -> Char -> Positive (Small Int) -> Positive (Small Int) -> Bool
nVectorConstructorValues c1 c2 s1 s2 = 
    let size1 = getSmall $ getPositive s1
        size2 = getSmall $ getPositive s2
        v :: Tensor Double = NVector.fromIndices [c1,c2] [size1,size2] (\[x,y] -> fromIntegral x + fromIntegral y)
        vConst :: Tensor Double = NVector.const [c1,c2] [size1,size2] (fromIntegral size1)
    in  c1 == c2 || (
        all (\(i1,i2) -> v $$| ([c1,c2],[i1,i2]) == fromIntegral i1 + fromIntegral i2) 
            (pure (,) <*> [0 .. size1 - 1] <*> [0 .. size2 - 1]) && 
        all (\(i1,i2) -> vConst $$| ([c1,c2],[i1,i2]) == fromIntegral size1) 
            (pure (,) <*> [0 .. size1 - 1] <*> [0 .. size2 - 1])
        )

-- | Test generic NForm constructor values
nFormConstructorValues :: Char -> Char -> Positive (Small Int) -> Positive (Small Int) -> Bool
nFormConstructorValues c1 c2 s1 s2 = 
    let size1 = getSmall $ getPositive s1
        size2 = getSmall $ getPositive s2
        v :: Tensor Double = NForm.fromIndices [c1,c2] [size1,size2] (\[x,y] -> fromIntegral x + fromIntegral y)
        vConst :: Tensor Double = NForm.const [c1,c2] [size1,size2] (fromIntegral size1)
    in  c1 == c2 || (
        all (\(i1,i2) -> v $$| ([c1,c2],[i1,i2]) == fromIntegral i1 + fromIntegral i2) 
            (pure (,) <*> [0 .. size1 - 1] <*> [0 .. size2 - 1]) && 
        all (\(i1,i2) -> vConst $$| ([c1,c2],[i1,i2]) == fromIntegral size1) 
            (pure (,) <*> [0 .. size1 - 1] <*> [0 .. size2 - 1])
        )

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
        \(t1 :: Tensor Double, t2 :: Tensor Double) -> collect (length $ _contractedIndices t1 t2) $ preserveIndicesBinary (+)

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

    executePropertyTest "vectorConstructor"  defTestN vectorConstructor
    executePropertyTest "formConstructor"    defTestN formConstructor
    executePropertyTest "matrixConstructor"  defTestN matrixConstructor
    executePropertyTest "nFormConstructor"   defTestN nFormConstructor
    executePropertyTest "nVectorConstructor" defTestN nVectorConstructor

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
