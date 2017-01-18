-----------------------------------------------------------------------------
--
-- Package     :  Tensor
-- Module      :  Main
-- Author      :  Artur M. Brodzki, Warsaw 2016
-----------------------------------------------------------------------------

module Main (
    main
) where

import           Data.Bits
import           Data.Vector        as V
import           Prelude            as P
import           Tensor
import           Tensor.Multilinear as T

m1 :: Tensor Int
m1 = T.generate (Covariant 1000 "j")
        (\i -> T.generate (Contravariant 1000 "i") (\j -> Scalar $ i - j `mod` 10))

m2 :: Tensor Int
m2 = T.generate (Contravariant 1000 "j")
        (\i -> T.generate (Covariant 1000 "k") (\j -> Scalar $ i - j `mod` 10))

m3 :: Tensor Int
m3 = m1 !* m2

v :: Tensor Int
v = T.generate (Contravariant 1000 "k") (\i -> Scalar $ i `mod` 10)

v2 :: Tensor Int
v2 = m3 !* v
{-
m01 :: Tensor Int
m01 = T.generate (Contravariant 1000 "j")
        (\j -> T.generate (Covariant 1000 "i") (\i -> Scalar $ i - j `mod` 10))

v01 :: Tensor Int
v01 = T.generate (Contravariant 1000 "i") (\i -> Scalar $ i `mod` 10)

v02 :: Tensor Int
v02 = T.generate (Contravariant 1000 "j") (\i -> Scalar $ i `mod` 10)

vr :: Tensor Int
vr = T.generate (Contravariant 10 "i") (\i -> if i == 9 then 3 else 2 * vr T.! (i + 1))
-}
main :: IO ()
main = do
    let v1 = [x .|. complement 3 | x <- [1..100000000]] :: [Int]
    let v2 = [x .|. complement 3 | x <- [1..100000000]] :: [Int]
    putStrLn "generated..."
    print $ P.sum $ P.zipWith (*) v1 v2

myZip :: Num a => [a] -> [a] -> [a]
myZip [] _            = []
myZip _ []            = []
myZip (a:as) (b : bs) = (a * b) : myZip as bs

