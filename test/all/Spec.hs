{-|
Module      : Main
Description : All tests suite for Multilinear library
Copyright   : (c) Artur M. Brodzki, 2018
License     : BSD3
Maintainer  : artur@brodzki.org
Stability   : experimental
Portability : Windows/POSIX

-}

import qualified GPU       
import qualified MultiCore
import qualified Sequential

-- | ENTRY POINT
main :: IO ()
main = do
    GPU.main
    MultiCore.main
    Sequential.main
