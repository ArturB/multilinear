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
import           Multilinear.Generic
import qualified Multilinear.Matrix.AsArray as Matrix.AsArray
import qualified Multilinear.Matrix.AsList  as Matrix.AsList
import qualified Multilinear.Vector.AsArray as Vector.AsArray
import qualified Multilinear.Vector.AsList  as Vector.AsList

ml1 :: ListTensor Int
ml1 = Matrix.AsList.fromIndices "ij" 500 500 $ \i j -> i + j

ml2 :: ListTensor Int
ml2 = Matrix.AsList.fromIndices "jk" 500 500 $ \j k -> j + k

vl :: ListTensor Int
vl = Vector.AsList.fromIndices "k" 500 id

vl2 :: ListTensor Int
vl2 = Vector.AsList.fromIndices "j" 500 id
{-
ma1 :: VectorTensor Int
ma1 = Matrix.AsArray.fromIndices "ij" 500 500 $ \i j -> i + j

ma2 :: VectorTensor Int
ma2 = Matrix.AsArray.fromIndices "jk" 500 500 $ \j k -> j + k

va :: VectorTensor Int
va = Vector.AsArray.fromIndices "k" 500 id

va2 :: VectorTensor Int
va2 = Vector.AsArray.fromIndices "j" 500 id
-}
main :: IO ()
main = do
    mmList  <- Meas.measure ( nfIO $ print (ml1 * ml2 * vl) ) 1
    mvList  <- Meas.measure ( nfIO $ print (ml1 * vl2     ) ) 1
    --mmArray <- Meas.measure ( nfIO $ print (ma1 * ma2 * va) ) 1
    --mvArray <- Meas.measure ( nfIO $ print (ma1 * va2     ) ) 1
    putStrLn $ "\nMultiply list matrix by matrix: "   ++ show (measCpuTime $ fst mmList)  ++ "s"
    putStrLn $ "\nMultiply list matrix by vector: "   ++ show (measCpuTime $ fst mvList)  ++ "s"
    --putStrLn $ "\nMultiply array matrix by array matrix: " ++ show (measCpuTime $ fst mmArray) ++ "s"
    --putStrLn $ "\nMultiply array matrix by array vector: " ++ show (measCpuTime $ fst mvArray) ++ "s"
    return ()

