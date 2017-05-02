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

main :: IO ()
main = do
    putStrLn "Start..."
    let res = ml1 * ml2 * vl
    print $ (res \/ "i") * res
    putStr "End..."



