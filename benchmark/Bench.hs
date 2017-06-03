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
import           Criterion.Measurement as Meas
import           Criterion.Types
import           Multilinear
import           Multilinear.Generic
import qualified Multilinear.Matrix    as Matrix
import qualified Multilinear.Vector    as Vector

m1 :: Tensor Int
m1 = Matrix.fromIndices "ij" 100 100 $ \i j -> i + j

m2 :: Tensor Int
m2 = Matrix.fromIndices "jk" 100 100 $ \j k -> j + k

v :: Tensor Int
v = Vector.fromIndices "k" 100 id

v2 :: Tensor Int
v2 = Vector.fromIndices "j" 100 id


main :: IO ()
main = do
    let m1l = m1 |>>> "j" :: Tensor Int
    let m2l = m2 |>>> "j" :: Tensor Int
    putStrLn "Matrix by matrix multiplying..."
    mmList  <- Meas.measure ( nfIO $ print $ m1l * m2l * v ) 1
    putStrLn "Matrix by vector multiplying..."
    mvList  <- Meas.measure ( nfIO $ print $ m1l * v2      ) 1
    putStrLn $ "\nMultiply matrix by matrix: "   ++ show (measCpuTime $ fst mmList)  ++ "s"
    putStrLn $ "\nMultiply matrix by vector: "   ++ show (measCpuTime $ fst mvList)  ++ "s"
    return ()

