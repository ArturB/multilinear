-----------------------------------------------------------------------------
--
-- Package     :  Tensor
-- Module      :  Main
-- Author      :  Artur M. Brodzki, Warsaw 2016
-----------------------------------------------------------------------------

module Main (
    main
) where

import           Prelude            as P
import           Tensor
import           Tensor.Multilinear as T

m1 :: Tensor Int
m1 = T.generate (Contravariant 500 "i")
        (\i -> T.generate (Covariant 50 "j") (\j -> Scalar $ i + j `mod` 10))

m2 :: Tensor Int
m2 = T.generate (Contravariant 500 "j")
        (\i -> T.generate (Covariant 500 "k") (\j -> Scalar $ i + j `mod` 10))

m3 :: Tensor Int
m3 = m1 !* m2

v :: Tensor Int
v = T.generate (Contravariant 50 "k") (\i -> Scalar $ i `mod` 10)

v2 :: Tensor Int
v2 = m3 !* v

main :: IO ()
main = do
        putStrLn "Starting..."
        print $ v2

