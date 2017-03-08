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
m1 = tensor ("i",[1000]) ("j",[1000]) $ \[i] [j] -> i+j

m2 :: Tensor Int Int
m2 = tensor ("j",[1000]) ("k",[1000]) $ \[i] [j] -> i+j

v :: Tensor Int Int
v = tensor ("k",[1000]) ([],[]) $ \[k] _ -> k

main :: IO ()
main = do
    putStrLn "Start..."
    print $ m1 * m2 * v
    putStrLn "End."

