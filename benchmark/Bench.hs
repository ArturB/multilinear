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
import           Criterion.Measurement      as Meas
import           Criterion.Types
import qualified Data.Vector                as Boxed
import qualified Data.Vector.Unboxed        as Unboxed
import           Multilinear
import           Multilinear.Generic
import           Multilinear.Generic.AsList
import qualified Multilinear.Matrix.AsArray as Matrix.AsArray
import qualified Multilinear.Matrix.AsList  as Matrix.AsList
import qualified Multilinear.Vector.AsArray as Vector.AsArray
import qualified Multilinear.Vector.AsList  as Vector.AsList

ml1 :: ListTensor Int
ml1 = Matrix.AsList.fromIndices "ij" 200 200 $ \i j -> i + j

ml2 :: ListTensor Int
ml2 = Matrix.AsList.fromIndices "jk" 200 200 $ \j k -> j + k

vl :: ListTensor Int
vl = Vector.AsList.fromIndices "k" 200 id

vl2 :: ListTensor Int
vl2 = Vector.AsList.fromIndices "j" 200 id

ma1 :: VectorTensor Int
ma1 = Matrix.AsArray.fromIndices "ij" 200 200 $ \i j -> i + j

ma2 :: VectorTensor Int
ma2 = Matrix.AsArray.fromIndices "jk" 200 200 $ \j k -> j + k

va :: VectorTensor Int
va = Vector.AsArray.fromIndices "k" 200 id

va2 :: VectorTensor Int
va2 = Vector.AsArray.fromIndices "j" 200 id

main :: IO ()
main = do
    {-let m1l = ml1 |>>> "j" :: ListTensor Int
    let m2l = ml2 |>>> "j" :: ListTensor Int
    let m1a = ma1 |>>> "j" :: VectorTensor Int
    let m2a = ma2 |>>> "j" :: VectorTensor Int-}
    let zl = Unboxed.generate 10000000 id
    --mmList  <- Meas.measure ( nfIO $ print $ Unboxed.sum $ Unboxed.zipWith (*) zl zl  ) 1
    print $ Unboxed.sum $ Unboxed.zipWith (*) zl zl
    {-mvList  <- Meas.measure ( nfIO $ print $ m1l * vl2      ) 1
    mmArray <- Meas.measure ( nfIO $ print $ m1a * m2a * va ) 1
    mvArray <- Meas.measure ( nfIO $ print $ m1a * va2      ) 1-}
    --putStrLn $ "\nMultiply list matrix by matrix: "   ++ show (measCpuTime $ fst mmList)  ++ "s"
    {-putStrLn $ "\nMultiply list matrix by vector: "   ++ show (measCpuTime $ fst mvList)  ++ "s"
    putStrLn $ "\nMultiply array matrix by array matrix: " ++ show (measCpuTime $ fst mmArray) ++ "s"
    putStrLn $ "\nMultiply array matrix by array vector: " ++ show (measCpuTime $ fst mvArray) ++ "s"-}
    return ()

