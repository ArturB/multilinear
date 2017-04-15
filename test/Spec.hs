{-|
Module      : Spec/Main
Description : Tests specification of Multilinear library
Copyright   : (c) Artur M. Brodzki, 2017
License     : 3-clause BSD
Maintainer  : artur.brodzki@gmail.com
Stability   : experimental
Portability : Windows/POSIX

-}

module Main (
    main
) where

import qualified Multilinear.Tensor         as Tensor
import qualified Multilinear.Generic.AsList as List
import           Prelude                    as P
import           Multilinear.Library

m1 :: List.Tensor Int Int
m1 = Tensor.fromIndices ("i",[100]) ("j",[100]) $ \[i] [j] -> i+j

m2 :: List.Tensor Int Int
m2 = Tensor.fromIndices ("j",[100]) ("k",[100]) $ \[i] [j] -> i+j

v :: List.Tensor Int Int
v = Tensor.fromIndices ("k",[100]) ([],[]) $ \[k] _ -> k

main :: IO ()
main = do
    putStrLn "Start..."
    print $ m1 * m2 * v
    putStrLn "End."

