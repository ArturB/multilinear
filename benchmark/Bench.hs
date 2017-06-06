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

import           Control.DeepSeq
import           Criterion.Main
import           Criterion.Measurement      as Meas
import           Criterion.Types
import           Data.Bits
import           Data.Vector                as Boxed
import           Multilinear
import qualified Multilinear.Form           as Form
import           Multilinear.Generic
import           Multilinear.Index          as Index
import           Multilinear.Index.Finite   as Finite
import           Multilinear.Index.Infinite as Infinite
import qualified Multilinear.Matrix         as Matrix
import qualified Multilinear.Vector         as Vector
import           Statistics.Distribution.Geometric

incompatibleTypes :: String
incompatibleTypes = "Incompatible tensor types!"

scalarIndices :: String
scalarIndices = "Scalar has no indices!"

invalidIndices :: String
invalidIndices = "Indices and its sizes not compatible with structure of linear functional!"

m1 :: Tensor Int
m1 = Matrix.fromIndices "ij" 1000 1000 $ \i j -> i + j

m2 :: Tensor Int
m2 = Matrix.fromIndices "jk" 1000 1000 $ \j k -> j + k

m1R :: IO (Tensor Int)
m1R = Matrix.randomInt "ij" 1000 1000 (geometric 0.3)

m2R :: IO (Tensor Int)
m2R = Matrix.randomInt "jk" 1000 1000 (geometric 0.3)

v :: Tensor Int
v = Vector.fromIndices "k" 1000 id

f :: Tensor Int
f = Form.fromIndices "k" 1000 id

v2 :: Tensor Int
v2 = Vector.fromIndices "j" 1000 id

form :: Tensor Int
form = SimpleFinite (Finite.Covariant 10000000000 "k") $ Boxed.generate 10000000000 id

vec :: Tensor Int
vec = SimpleFinite (Finite.Contravariant 10000000000 "k") $ Boxed.generate 10000000000 id

--mult :: Num a => Bits a => Tensor a -> Tensor a -> Tensor a
--t1 `mult` t2 = _elemByElem t1 t2 (*) dot

main :: IO ()
main = do
    --m1Rm <- m1R
    --m2Rm <- m2R
    let m1l = m1 |>>> "j"
    let m2l = m2 |>>> "j"
    {-putStrLn "Matrix by matrix multiplying..."
    mmList  <- Meas.measure ( nfIO $ print $ m1l `mult` m2l `mult` v ) 1
    putStrLn "Matrix by vector multiplying..."
    mvList <- Meas.measure ( nfIO $ print $ m1l * v2       ) 1
    putStrLn $ "\nMultiply matrix by matrix: " Prelude.++ show (measCpuTime $ fst mmList) Prelude.++ "s"
    putStrLn $ "\nMultiply matrix by vector: " Prelude.++ show (measCpuTime $ fst mvList) Prelude.++ "s"-}
    (m1l .*. m2l) `deepseq` putStrLn "End!"
    return ()

