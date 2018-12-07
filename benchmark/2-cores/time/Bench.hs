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
import           Criterion.Types
import           Multilinear.Generic.MultiCore
import qualified Multilinear.Matrix                  as Matrix

-- | Simple generator function for bencharking matrices
gen :: Int -> Int -> Double
gen j k = sin (fromIntegral j) + cos (fromIntegral k)

-- | Generate benchmark of matrix multiplication
sizedMatrixMultBench :: 
    Int -- ^ size of square matrix to multiplicate
 -> Benchmark
sizedMatrixMultBench s = 
    bench (show s ++ "x" ++ show s) $ 
        nf ((Matrix.fromIndices "ij" s s gen :: Tensor Double) *) (Matrix.fromIndices "jk" s s gen :: Tensor Double)

-- | Generate benchmark of matrix addition
sizedMatrixAddBench :: 
    Int -- ^ size of square matrix to add
 -> Benchmark
sizedMatrixAddBench s = 
    bench (show s ++ "x" ++ show s) $ 
        nf ((Matrix.fromIndices "ij" s s gen :: Tensor Double) +) (Matrix.fromIndices "ij" s s gen :: Tensor Double)

-- | ENTRY POINT
main :: IO ()
main = defaultMainWith defaultConfig { reportFile = Just "benchmark/2-cores-bench.html" } [
    bgroup "matrix addition" $ sizedMatrixAddBench <$> [64, 128, 256, 512, 1024],
    bgroup "matrix multiplication" $ sizedMatrixMultBench <$> [64, 128, 256, 512, 1024]
    ]
