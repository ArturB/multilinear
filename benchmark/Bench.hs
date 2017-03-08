{-|
Module      : Spec/Main
Description : Benchmark of Multilinear library
Copyright   : (c) Artur M. Brodzki, 2017
License     : 3-clause BSD
Maintainer  : artur.brodzki@gmail.com
Stability   : experimental
Portability : Windows/POSIX

-}

module Main (
    main
) where

import           Multilinear
import           Criterion.Main
import           Criterion.Measurement as Meas
import           Criterion.Types

m1 :: Tensor Int Int
m1 = tensor ("i",[500]) ("j",[500]) $ \[i] [j] -> i+j

m2 :: Tensor Int Int
m2 = tensor ("j",[500]) ("k",[500]) $ \[i] [j] -> i+j

v :: Tensor Int Int
v = vector "k" 500 id

v2 :: Tensor Int Int
v2 = form "i" 500 id

main :: IO ()
main = do
    result <- Meas.measure ( nfIO $ print (v2 * (m1 * m2 * v)) ) 1
    putStrLn $ "\nTime: " ++ show (measCpuTime $ fst result) ++ "s\n"
    return ()

