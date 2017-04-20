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

import qualified Multilinear.Form    as Form
import           Multilinear.Generic
import           Multilinear.Generic.AsList
import qualified Multilinear.Matrix  as Matrix
import qualified Multilinear.Vector  as Vector

m1 :: ListTensor Int
m1 = Matrix.fromIndices "ij" 10000 10000 $ \i k -> i + k

m2 :: ListTensor Int
m2 = Matrix.fromIndices "jk" 1000 1000 $ \j k -> j + k

v :: ListTensor Int
v = Vector.fromIndices "k" 1000 id

v2 :: ListTensor Int
v2 = Form.const "l" 5 10

main :: IO ()
main = do
    putStrLn "Start..."
    putStr "End..."



