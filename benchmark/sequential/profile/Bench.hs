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

import           Control.DeepSeq
import           Multilinear
import qualified Multilinear.Matrix as Matrix

gen :: Int -> Int -> Double
gen j k = sin (fromIntegral j) + cos (fromIntegral k)

main :: IO ()
main = do
    let m = (Matrix.fromIndices "ij" 1000 1000 gen :: Tensor Double) + (Matrix.fromIndices "jk" 1000 1000 gen :: Tensor Double)
    m `deepseq` putStrLn $ "All done! Indices of m:" ++ show (indices m)
