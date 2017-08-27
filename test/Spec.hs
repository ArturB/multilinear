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
import qualified Multilinear.Form   as Form
import qualified Multilinear.Matrix as Matrix
import qualified Multilinear.Vector as Vector
import qualified Multilinear.Tensor as Tensor
--import qualified Multilinear.Form as Form
--import           Statistics.Distribution.Normal

ml1 :: Tensor Int
ml1 = Matrix.fromIndices "ij" 1000 1000 $ \i j -> i * j

ml2 :: Tensor Int
ml2 = Matrix.fromIndices "jk" 1000 1000 $ \j k -> j * k

vl :: Tensor Int
vl = Vector.fromIndices "k" 1000 id 

--vl2 :: Tensor Int
--vl2 = Vector.fromIndices "j" 500 id

main :: IO ()
main = do
    putStrLn "Start..."
    print $ ml1 * ml2 * vl
    putStr "End..."



