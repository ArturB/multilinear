{-|
Module      : Bench
Description : Benchmark of Multilinear library
Copyright   : (c) Artur M. Brodzki, 2018
License     : BSD3
Maintainer  : artur@brodzki.org
Stability   : experimental
Portability : Windows/POSIX

-}

module Main (
    main
) where

import           Weigh
import qualified Multilinear.Matrix as Matrix
import qualified Multilinear.Vector as Vector

gen :: Int -> Int -> Double
gen j k = sin (fromIntegral j) + cos (fromIntegral k)

main :: IO ()
main = mainWith (do
    func "vector 1 elem generation" (Vector.fromIndices "i" 1) id
    func "vector 2 elem generation" (Vector.fromIndices "i" 2) id
    func "vector 3 elem generation" (Vector.fromIndices "i" 3) id
    func "vector 4 elem generation" (Vector.fromIndices "i" 4) id
    func "vector 5 elem generation" (Vector.fromIndices "i" 5) id
    func "matrix 1024x1024 generation" (Matrix.fromIndices "ij" 1024 1024) gen
    func "matrix 1024x1024 multiplication" (* Matrix.fromIndices "jk" 1024 1024 gen) (Matrix.fromIndices "ij" 1024 1024 gen)
    )
