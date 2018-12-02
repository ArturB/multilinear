{-|
Module      : Bench
Description : Memory benchmark of Multilinear library
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
import           Multilinear.Generic.GPU
import qualified Multilinear.Matrix as Matrix
import qualified Multilinear.Vector as Vector

-- | Simple generator function for benchmarked matrices
gen :: Int -> Int -> Double
gen j k = sin (fromIntegral j) + cos (fromIntegral k)

-- matrix sizes
s1 :: Int
s1 = 64
s2 :: Int
s2 = 256
s3 :: Int
s3 = 1024

-- | ENTRY POINT
main :: IO ()
main = mainWith (do
    setColumns [Case, Allocated, GCs, Live, Max]

    -- Benchmarking small vectors
    value "vector 1 elem generation" (Vector.fromIndices "i" 1 fromIntegral :: Tensor Double)
    value "vector 2 elem generation" (Vector.fromIndices "i" 2 fromIntegral :: Tensor Double)
    value "vector 3 elem generation" (Vector.fromIndices "i" 3 fromIntegral :: Tensor Double)

    -- Benchmarking matrix generators
    value "matrix 64 x 64 generation" 
        (Matrix.fromIndices "ij" s1 s1 gen :: Tensor Double)
    value "matrix 256 x 256 generation" 
        (Matrix.fromIndices "ij" s2 s2 gen :: Tensor Double)
    value "matrix 1024 x 1024 generation" 
        (Matrix.fromIndices "ij" s3 s3 gen :: Tensor Double)

    -- Benchmarking matrix addition
    func "matrix 64 x 64 addition"
        (+ Matrix.fromIndices "ab" s1 s1 gen)
        (Matrix.fromIndices "ab" s1 s1 (\a b -> fromIntegral a + fromIntegral b) :: Tensor Double)
    func "matrix 256 x 256 addition"
        (+ Matrix.fromIndices "ab" s2 s2 gen)
        (Matrix.fromIndices "ab" s2 s2 (\a b -> fromIntegral a + fromIntegral b) :: Tensor Double)
    func "matrix 1024 x 1024 addition"
        (+ Matrix.fromIndices "ab" s3 s3 gen)
        (Matrix.fromIndices "ab" s3 s3 (\a b -> fromIntegral a + fromIntegral b) :: Tensor Double)
    
    -- Benchmarking matrix multiplication
    func "matrix 40 x 4,000 multiplication"
        (* Matrix.fromIndices "jk" 4000 40 gen)
        (Matrix.fromIndices "ij" 40 4000 gen :: Tensor Double)
    func "matrix 40 x 16,000 multiplication"
        (* Matrix.fromIndices "jk" 16000 40 gen)
        (Matrix.fromIndices "ij" 40 16000 gen :: Tensor Double)
    func "matrix 40 x 64,000 multiplication"
        (* Matrix.fromIndices "jk" 64000 40 gen)
        (Matrix.fromIndices "ij" 40 64000 gen :: Tensor Double)
    )
