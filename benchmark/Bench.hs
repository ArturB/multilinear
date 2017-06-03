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

import           Control.Applicative
import           Criterion.Main
import           Criterion.Measurement as Meas
import           Criterion.Types
import qualified Data.Vector           as Boxed
import qualified Data.Vector.Unboxed   as Unboxed
import           Multilinear
import           Multilinear.Generic
import qualified Multilinear.Matrix    as Matrix
import qualified Multilinear.Vector    as Vector

m1 :: ListTensor Int
m1 = Matrix.fromIndices "ij" 500 500 $ \i j -> i + j

m2 :: ListTensor Int
m2 = Matrix.fromIndices "jk" 500 500 $ \j k -> j + k

v :: ListTensor Int
v = Vector.fromIndices "k" 500 id

v2 :: ListTensor Int
v2 = Vector.fromIndices "j" 500 id


main :: IO ()
main = do
    let m1l = ml |>>> "j" :: Tensor Int
    let m2l = ml |>>> "j" :: Tensor Int
    mmList  <- Meas.measure ( m1l * m2l * v `deepseq` print "Matrix by matrix multiplying..." ) 1
    mvList  <- Meas.measure ( m1l * v2 `deepseq` print "Matrix by vector multiplying..."      ) 1
    putStrLn $ "\nMultiply matrix by matrix: "   ++ show (measCpuTime $ fst mmList)  ++ "s"
    putStrLn $ "\nMultiply matrix by vector: "   ++ show (measCpuTime $ fst mvList)  ++ "s"
    return ()

