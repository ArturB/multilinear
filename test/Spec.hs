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

import           Multilinear
import           Prelude     as P

m1 :: Tensor Int Int
m1 = tensor ("i",[100]) ("j",[100]) $ \[i] [j] -> i+j

m2 :: Tensor Int Int
m2 = tensor ("j",[100]) ("k",[100]) $ \[i] [j] -> i+j

v :: Tensor Int Int
v = tensor ("k",[100]) ([],[]) $ \[k] _ -> k

main :: IO ()
main = do
    putStrLn "Start..."
    print $ m1 * m2 * v
    putStrLn "End."

