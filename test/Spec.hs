{-|
Module      : Main
Description : Test of Multilinear library
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
import           Multilinear.Generic
import           Multilinear.Class
import qualified Multilinear.Matrix as Matrix
import qualified Multilinear.Vector as Vector

v_i :: Tensor Double
v_i = Vector.fromIndices "i" 10 fromIntegral

v_j :: Tensor Double
v_j = Vector.fromIndices "j" 5 (\x -> fromIntegral x + 5.0)

v_k :: Tensor Double
v_k = Vector.fromIndices "k" 10 fromIntegral

m_ik :: Tensor Double
m_ik = Matrix.fromIndices "ik" 10 10 (\i j -> fromIntegral i + fromIntegral j)

main :: IO ()
main = do
    putStr "v^i = "
    print v_i
    putStr "v^j = "
    print v_j
    putStr "v^k = "
    print v_k
    putStr "Matrix m_ji = v^j + v_i = "
    let m = v_j + (v_i \/ "i")
    print m
    putStr "m_ji * v^i = "
    print $ m * v_i
    putStr "m_ji * v^k = "
    print $ m * v_k
    putStr "Matrix m_ik = "
    print m_ik
    putStr "m_ik * v^k = "
    print $ m_ik * v_k
    putStr "m_ik |>>> i = "
    print $ m_ik |>>> "i"
    putStr "m_ji * m_ik"
    print $ m * m_ik
