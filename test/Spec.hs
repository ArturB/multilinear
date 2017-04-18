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

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Data.Maybe
import qualified Multilinear.Generic.AsList as List
import qualified Multilinear.Generic        as Generic
import           Multilinear.Index.Finite
import qualified Multilinear.Tensor         as Tensor
import           Prelude                    as P

m1 :: List.Tensor Int Int
m1 = Tensor.fromIndices ("i",[100]) ("j",[100]) $ \[i] [j] -> i+j

m2 :: List.Tensor Int Int
m2 = Tensor.fromIndices ("j",[100]) ("k",[100]) $ \[i] [j] -> i+j

v :: List.Tensor Int Int
v = Tensor.fromIndices ("k",[100]) ([],[]) $ \[k] _ -> k

ten1 :: Generic.Tensor [] Integer Integer
ten1 = Generic.FiniteTensor (Contravariant 10 "i") [Generic.Scalar x | x <- [2..101]]

ten2 = Generic.FiniteTensor (Contravariant 10 "i") [Generic.Scalar (x + 1) | x <- [1..100]]

--readTen = do
    

main :: IO ()
main = do
    putStrLn "Start..."
    print $ m1 * m2 * v
    putStrLn "End."

    Generic.toJSONFile "jsontest.json" ten1

    tenr <- runMaybeT $ Generic.fromJSONFile "jsontest.json"
    print $ tenr <= Just ((\e -> e - 1) <$> ten1)

    --print $ fromJust tenr
    

