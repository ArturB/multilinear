{-|
Module      : Bench
Description : Benchmark of Multilinear library
Copyright   : (c) Artur M. Brodzki, 2017
License     : GPL-3
Maintainer  : artur.brodzki@gmail.com
Stability   : experimental
Portability : Windows/POSIX

-}

module Main (
    main
) where

import           Criterion.Main
import           Criterion.Measurement      as Meas
import           Criterion.Types
import qualified Multilinear.Generic.AsList as List
import qualified Multilinear.Tensor         as Tensor
import qualified Multilinear.Vector         as Vector

m1 :: List.Tensor Int Int
m1 = Tensor.fromIndices ("i",[500]) ("j",[500]) $ \[i] [j] -> i+j

m2 :: List.Tensor Int Int
m2 = Tensor.fromIndices ("j",[500]) ("k",[500]) $ \[i] [j] -> i+j

v :: List.Tensor Int Int
v = Vector.fromIndices "k" 500 id

v2 :: List.Tensor Int Int
v2 = Vector.fromIndices "i" 500 id

main :: IO ()
main = do
    result <- Meas.measure ( nfIO $ print (v2 * (m1 * m2 * v)) ) 1
    putStrLn $ "\nTime: " ++ show (measCpuTime $ fst result) ++ "s\n"
    return ()

