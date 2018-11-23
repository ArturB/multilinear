{-|
Module      : Multilinear.Generic
Description : Re-export default tensor implementation
Copyright   : (c) Artur M. Brodzki, 2018
License     : BSD3
Maintainer  : artur@brodzki.org
Stability   : experimental
Portability : Windows/POSIX

-}

module Multilinear.Generic (
    module DefaultTensorImplementation
) where

import Multilinear.Generic.Sequential as DefaultTensorImplementation
