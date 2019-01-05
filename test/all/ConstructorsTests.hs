{-|
Module      : ConstructorsTests
Description : Test of tensor constructors
Copyright   : (c) Artur M. Brodzki, 2018
License     : BSD3
Maintainer  : artur@brodzki.org
Stability   : experimental
Portability : Windows/POSIX

-}

module ConstructorsTests (
    vectorConstructorValues, vectorConstructorError, 
    formConstructorValues, formConstructorError, 
    matrixConstructorValues, matrixConstructorError, 
    nVectorConstructorValues, nVectorConstructorError, 
    nFormConstructorValues, nFormConstructorError
) where

import           Multilinear.Class
import           Multilinear.Generic.GPU
import qualified Multilinear.Form               as Form
import qualified Multilinear.Matrix             as Matrix
import qualified Multilinear.Vector             as Vector
import qualified Multilinear.NForm              as NForm
import qualified Multilinear.NVector            as NVector
import qualified MultilinearTests
import           Test.QuickCheck
import           Test.QuickCheck.Common
import           Test.QuickCheck.Multilinear.Generic.GPU()


-- | Test generic vector constructor values
vectorConstructorValues :: Multilinear t a => Char -> Positive (Small Int) -> t a -> Bool
vectorConstructorValues c s _ = 
    let size = getSmall $ getPositive s
        v :: t a = Vector.fromIndices [c] size fromIntegral
        vConst :: t a = Vector.const [c] size (fromIntegral size)
    in  all (\i -> v $$| ([c],[i]) == fromIntegral i) [0 .. size - 1] && 
        all (\i -> vConst $$| ([c],[i]) == fromIntegral size) [0 .. size - 1]

-- | Test generic form constructor values
formConstructorValues :: Multilinear t a => Char -> Positive (Small Int) -> t a -> Bool
formConstructorValues c s _ = 
    let size = getSmall $ getPositive s
        f :: t a = Form.fromIndices [c] size fromIntegral
        fConst :: t a = Form.const [c] size (fromIntegral size)
    in  all (\i -> f $$| ([c],[i]) == fromIntegral i) [0.. size - 1] && 
        all (\i -> fConst $$| ([c],[i]) == fromIntegral size) [0 .. size - 1]

-- | Test generic matrix constructor values
matrixConstructorValues :: Multilinear t a => Char -> Char -> Positive (Small Int) -> Positive (Small Int) -> t a -> Bool
matrixConstructorValues c1 c2 s1 s2 _ = 
    let size1 = getSmall $ getPositive s1
        size2 = getSmall $ getPositive s2
        v :: t a = Matrix.fromIndices [c1,c2] size1 size2 (\x y -> fromIntegral x + fromIntegral y)
        vConst :: t a = Matrix.const [c1,c2] size1 size2 (fromIntegral size1)
    in  c1 == c2 || (
        all (\(i1,i2) -> v $$| ([c1,c2],[i1,i2]) == fromIntegral i1 + fromIntegral i2) 
            (pure (,) <*> [0 .. size1 - 1] <*> [0 .. size2 - 1]) && 
        all (\(i1,i2) -> vConst $$| ([c1,c2],[i1,i2]) == fromIntegral size1) 
            (pure (,) <*> [0 .. size1 - 1] <*> [0 .. size2 - 1])
        )

-- | Test generic NVector constructor values
nVectorConstructorValues :: Multilinear t a => Char -> Char -> Positive (Small Int) -> Positive (Small Int) -> t a -> Bool
nVectorConstructorValues c1 c2 s1 s2 _ = 
    let size1 = getSmall $ getPositive s1
        size2 = getSmall $ getPositive s2
        v :: t a = NVector.fromIndices [c1,c2] [size1,size2] (\[x,y] -> fromIntegral x + fromIntegral y)
        vConst :: t a = NVector.const [c1,c2] [size1,size2] (fromIntegral size1)
    in  c1 == c2 || (
        all (\(i1,i2) -> v $$| ([c1,c2],[i1,i2]) == fromIntegral i1 + fromIntegral i2) 
            (pure (,) <*> [0 .. size1 - 1] <*> [0 .. size2 - 1]) && 
        all (\(i1,i2) -> vConst $$| ([c1,c2],[i1,i2]) == fromIntegral size1) 
            (pure (,) <*> [0 .. size1 - 1] <*> [0 .. size2 - 1])
        )

-- | Test generic NForm constructor values
nFormConstructorValues :: Multilinear t a => Char -> Char -> Positive (Small Int) -> Positive (Small Int) -> t a -> Bool
nFormConstructorValues c1 c2 s1 s2 _ = 
    let size1 = getSmall $ getPositive s1
        size2 = getSmall $ getPositive s2
        v :: t a = NForm.fromIndices [c1,c2] [size1,size2] (\[x,y] -> fromIntegral x + fromIntegral y)
        vConst :: t a = NForm.const [c1,c2] [size1,size2] (fromIntegral size1)
    in  c1 == c2 || (
        all (\(i1,i2) -> v $$| ([c1,c2],[i1,i2]) == fromIntegral i1 + fromIntegral i2) 
            (pure (,) <*> [0 .. size1 - 1] <*> [0 .. size2 - 1]) && 
        all (\(i1,i2) -> vConst $$| ([c1,c2],[i1,i2]) == fromIntegral size1) 
            (pure (,) <*> [0 .. size1 - 1] <*> [0 .. size2 - 1])
        )


-- | Test generic vector constructor indices error
vectorConstructorError :: Multilinear t a => Char -> Positive (Small Int) -> t a -> Property
vectorConstructorError c s _ = 
    let size = getSmall $ getPositive s
        v :: t a = Vector.fromIndices [c,'a'] size fromIntegral
    in  expectFailure (total v)

-- | Test generic form constructor indices error
formConstructorError :: Multilinear t a => Char -> Positive (Small Int) -> t a -> Property
formConstructorError c s _ = 
    let size = getSmall $ getPositive s
        f :: t a = Form.fromIndices [c,'a'] size fromIntegral
    in  expectFailure (total f)

-- | Test generic matrix constructor indices error
matrixConstructorError :: Multilinear t a => Char -> Char -> Positive (Small Int) -> Positive (Small Int) -> t a -> Property
matrixConstructorError c1 c2 s1 s2 _ = 
    let size1 = getSmall $ getPositive s1
        size2 = getSmall $ getPositive s2
        v :: t a = Matrix.fromIndices [c1,c2,'a'] size1 size2 (\x y -> fromIntegral x + fromIntegral y)
    in  expectFailure (total v)

-- | Test generic NVector constructor indices error
nVectorConstructorError :: Multilinear t a => Char -> Char -> Positive (Small Int) -> Positive (Small Int) -> t a -> Property
nVectorConstructorError c1 c2 s1 s2 _ = 
    let size1 = getSmall $ getPositive s1
        size2 = getSmall $ getPositive s2
        v :: t a = NVector.fromIndices [c1,c2,'a'] [size1,size2] (\[x,y] -> fromIntegral x + fromIntegral y)
    in  expectFailure (total v)

-- | Test generic NForm constructor indices error
nFormConstructorError :: Multilinear t a => Char -> Char -> Positive (Small Int) -> Positive (Small Int) -> t a -> Property
nFormConstructorError c1 c2 s1 s2 _ = 
    let size1 = getSmall $ getPositive s1
        size2 = getSmall $ getPositive s2
        v :: t a = NForm.fromIndices [c1,c2,'a'] [size1,size2] (\[x,y] -> fromIntegral x + fromIntegral y)
    in  expectFailure (total v)
