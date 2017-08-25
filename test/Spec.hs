{-|
Module      : Spec
Description : Tests specification of Multilinear library
Copyright   : (c) Artur M. Brodzki, 2017
License     : GPL-3
Maintainer  : artur.brodzki@gmail.com
Stability   : experimental
Portability : Windows/POSIX

-}

module Main (
    main
) where

import           Multilinear
import           Multilinear.Generic
import qualified Multilinear.Matrix as Matrix
import qualified Multilinear.Vector as Vector
--import qualified Multilinear.Form as Form
--import           Statistics.Distribution.Normal

ml1 :: Tensor Int
ml1 = Matrix.fromIndices "ij" 5 5 $ \i j -> i + j

ml2 :: Tensor Int
ml2 = Matrix.fromIndices "jk" 5 5 $ \j k -> j + k

vl :: Tensor Int
vl = Vector.fromIndices "k" 5 id 

--vl2 :: Tensor Int
--vl2 = Vector.fromIndices "j" 500 id

main :: IO ()
main = do
    putStrLn "Start..."
    let m2 = ml2 |>>> "j"
    --let res = ml1 * m2 * vl
    print $ ml1 * m2
    print vl
    putStr "End..."



