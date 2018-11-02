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

import           Criterion.Main
import qualified Multilinear.Matrix                  as Matrix

main :: IO ()
main = putStrLn "Test to do..."