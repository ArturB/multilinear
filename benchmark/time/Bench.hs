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

import           Criterion.Main
import qualified Multilinear.Matrix                  as Matrix

gen :: Int -> Int -> Double
gen j k = sin (fromIntegral j) + cos (fromIntegral k)

sizedMatrixMultBench :: Int -> Benchmark
sizedMatrixMultBench s = 
    bench ((show s) ++ "x" ++ (show s)) $ 
        nf ((Matrix.fromIndices "ij" s s gen) *) (Matrix.fromIndices "jk" s s gen)

sizedMatrixAddBench :: Int -> Benchmark
sizedMatrixAddBench s = 
    bench ((show s) ++ "x" ++ (show s)) $ 
        nf ((Matrix.fromIndices "ij" s s gen) +) (Matrix.fromIndices "ij" s s gen)

main :: IO ()
main = defaultMain [
    bgroup "matrix multiplication" $ sizedMatrixMultBench <$> [64, 128, 256, 512],
    bgroup "matrix addition" $ sizedMatrixAddBench <$> [64, 128, 256, 512]
    ]
