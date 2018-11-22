{-|
Module      : Multilinear.Generic
Description : Re-exports default tensor implementation
Copyright   : (c) Artur M. Brodzki, 2018
License     : BSD3
Maintainer  : artur@brodzki.org
Stability   : experimental
Portability : Windows/POSIX

-}

module Multilinear.Generic (
    module DefaultGenericTensor
) where

-- | Re-export default tensor implementation
import Multilinear.Generic.Sequential as DefaultGenericTensor
