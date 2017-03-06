{-|
Module      : Spec/Main
Description : Tests specification of Multilinear library
Copyright   : (c) Artur M. Brodzki, 2017
License     : 3-clause BSD
Maintainer  : artur.brodzki@gmail.com
Stability   : experimental
Portability : Windows/POSIX

-}

module Main (
    main
) where

import           Prelude            as P
--import Multilinear
{-
m1 :: Tensor Int
m1 = T.generate (Contravariant 500 "i")
        (\i -> T.generate (Covariant 500 "j") (\j -> Scalar $ i + j `mod` 10))

m2 :: Tensor Int
m2 = T.generate (Contravariant 500 "j")
        (\i -> T.generate (Covariant 500 "k") (\j -> Scalar $ i + j `mod` 10))

m3 :: Tensor Int
m3 = m1 !* m2

v :: Tensor Int
v = T.generate (Contravariant 500 "k") (\i -> Scalar $ i `mod` 10)

v2 :: Tensor Int
v2 = m3 !* v
-}
{-| Entry point of all library tests -}
main :: IO ()
main = do
        putStrLn "Starting..."
        putStrLn "End."

