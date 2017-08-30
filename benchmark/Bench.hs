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
import           Criterion.Measurement               as Meas
import           Criterion.Types
import           Multilinear.Generic
import qualified Multilinear.Matrix                  as Matrix

m1 :: Tensor Double
m1 = Matrix.fromIndices "ij" 1000 1000 $ \i j -> fromIntegral (2*i) - exp (fromIntegral j)

m2 :: Tensor Double
m2 = Matrix.fromIndices "jk" 1000 1000 $ \i j -> sin (fromIntegral i) + cos (fromIntegral j)

main :: IO ()
main = do
    putStrLn "Two matrices 1000x1000 multiplying..."
    (meas,_)  <- Meas.measure ( nfIO $ (m1 * m2) `deepseq` putStrLn "End!" ) 1
    putStrLn $ "Measured time: " ++ show (measCpuTime meas) ++ " s."
    return ()

