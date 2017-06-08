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
import           Criterion.Measurement              as Meas
import           Criterion.Types
import           Data.Bits
import qualified Data.Set                           as Set
import           Data.Vector                        as Boxed
import           Multilinear
import qualified Multilinear.Form                    as Form
import           Multilinear.Generic
import qualified Mutlilinear.Parallel.Generic        as Parallel
import           Multilinear.Index                   as Index
import           Multilinear.Index.Finite            as Finite
import           Multilinear.Index.Infinite          as Infinite
import qualified Multilinear.Matrix                  as Matrix
import qualified Multilinear.Vector                  as Vector
import qualified Multilinear.Tensor                  as Tensor
import qualified Multilinear.Parallel.Matrix         as Parallel.Matrix
import qualified Multilinear.Parallel.Vector         as Parallel.Vector
import qualified Multilinear.Parallel.Tensor         as Parallel.Tensor
import           Statistics.Distribution.Normal
import           Statistics.Distribution.Exponential
import           Statistics.Distribution.Geometric

incompatibleTypes :: String
incompatibleTypes = "Incompatible tensor types!"

scalarIndices :: String
scalarIndices = "Scalar has no indices!"

invalidIndices :: String
invalidIndices = "Indices and its sizes not compatible with structure of linear functional!"

m1R :: IO (Tensor Double)
m1R = Matrix.randomDouble "ij" 1000 1000 (exponential 0.5)

m2R :: IO (Tensor Double)
m2R = Matrix.randomDouble "ij" 1000 1000 (exponential 0.5)

m1D :: Tensor Double
m1D = Matrix.fromIndices "ij" 3000 3000 (\i j -> fromIntegral $ i + j)

m2D :: Tensor Double
m2D = Matrix.fromIndices "ij" 3000 3000 (\i j -> fromIntegral $ i * j)

t1R :: IO (Tensor Int)
t1R = Tensor.randomInt ("i", [1000]) ("j", [1000]) (geometric 0.5)

t2R :: IO (Tensor Int)
t2R = Tensor.randomInt ("j", [1000]) ("k", [1000]) (geometric 0.5)

m1P :: IO (Parallel.Tensor Double)
m1P = Parallel.Matrix.randomDouble "ij" 1000 1000 (exponential 0.5)

m2P :: IO (Parallel.Tensor Double)
m2P = Parallel.Matrix.randomDouble "jk" 1000 1000 (exponential 0.5)

v2 :: Tensor Int
v2 = Vector.fromIndices "j" 1000 id

--mult :: Num a => Bits a => Tensor a -> Tensor a -> Tensor a
--t1 `mult` t2 = _elemByElem t1 t2 (*) dot

main :: IO ()
main = do
    m1 <- m1P
    m2 <- m2P
    {-putStrLn "Matrix by matrix multiplying..."
    mmList  <- Meas.measure ( nfIO $ print $ m1l `mult` m2l `mult` v ) 1
    putStrLn "Matrix by vector multiplying..."
    mvList <- Meas.measure ( nfIO $ print $ m1l * v2       ) 1
    putStrLn $ "\nMultiply matrix by matrix: " Prelude.++ show (measCpuTime $ fst mmList) Prelude.++ "s"
    putStrLn $ "\nMultiply matrix by vector: " Prelude.++ show (measCpuTime $ fst mvList) Prelude.++ "s"-}
    --print commonIndices
    (m1 * m2) `deepseq` putStrLn "End!"
    --vec <- v
    --print vec
    --m1l `deepseq` putStrLn "End!"
    return ()

