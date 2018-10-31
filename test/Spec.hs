{-|
Module      : Main (Spec.hs)
Description : Tests of Multilinear library
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
import           Criterion.Main
import           Criterion.Measurement               as Meas
import           Criterion.Types
import           Multilinear.Generic
import qualified Multilinear.Matrix                  as Matrix

m1 :: Tensor Double
m1 = Matrix.fromIndices "ij" 100 100 $ \i j -> fromIntegral (2*i) - exp (fromIntegral j)

m2 :: Tensor Double
m2 = Matrix.fromIndices "jk" 100 100 $ \i j -> sin (fromIntegral i) + cos (fromIntegral j)

main :: IO ()
main = do
  putStrLn "Two matrices 1000x1000 multiplying..."
  (meas,_)  <- Meas.measure ( nfIO $ (m1 * m2) `deepseq` putStrLn "End!" ) 1
  putStrLn $ "Measured time: " ++ show (measCpuTime meas) ++ " s."
  return ()

