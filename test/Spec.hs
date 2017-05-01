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

import qualified Multilinear.Form.AsArray   as Form
import           Multilinear.Generic
import qualified Multilinear.Matrix.AsArray as Matrix
import qualified Multilinear.Vector.AsArray as Vector

m1 :: VectorTensor Int
m1 = Matrix.fromIndices "ij" 5 5 $ \i k -> i + k

m2 :: VectorTensor Int
m2 = Matrix.fromIndices "jk" 5 5 $ \j k -> j + k

v :: VectorTensor Int
v = Vector.fromIndices "k" 5 id

v2 :: VectorTensor Int
v2 = Form.const "l" 5 10

main :: IO ()
main = do
    putStrLn "Start..."
    print $ m1 * m2
    putStr "End..."



