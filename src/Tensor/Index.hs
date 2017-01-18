-----------------------------------------------------------------------------
--
-- Package     :  Tensor
-- Module      :  Tensor.Index
-- Description :  Defines index of tensor - covariant or contravariant
-- Author      :  Artur M. Brodzki, Warsaw 2016
-----------------------------------------------------------------------------

{-# LANGUAGE Strict #-}
{-# OPTIONS_GHC -O2 #-}

module Tensor.Index (
    TIndex(..),
    equivI, (!=!)
) where

import Data.Binary
--import Control.Lens

{- TENSOR INDEX -}
data TIndex =
    Covariant {
        indexCount :: Int,
        indexName :: String
    } |
    Contravariant {
        indexCount :: Int,
        indexName :: String
    } |
    Indifferent {
        indexCount :: Int,
        indexName :: String
    }
    deriving Eq

--makeLenses ''TIndex

-- Serialization instance
instance Binary TIndex where
    put (Covariant c n) = do
        put (0 :: Word8)
        put c
        put n

    put (Contravariant c n) = do
        put (1 :: Word8)
        put c
        put n
    put (Indifferent c n) = do
        put (2 :: Word8)
        put c
        put n

    get = do
        f <- get :: Get Word8
        c <- get
        n <- get
        if f == 0
        then
            return $ Covariant c n
        else
            return $ Contravariant c n

-- Show instance of TIndex
instance Show TIndex where
    show (Covariant c n) = "[" ++ n ++ ":" ++ show c ++ "]"
    show (Contravariant c n) = "<" ++ n ++ ":" ++ show c ++ ">"
    show (Indifferent c n) = "(" ++ n ++ ":" ++ show c ++ ")"

-- Returns true if two indices are quivalent, i.e. differs only by name
equivI :: TIndex -> TIndex -> Bool
equivI (Covariant count1 _) (Covariant count2 _)
    | count1 == count2 = True
    | otherwise = False
equivI (Contravariant count1 _) (Contravariant count2 _)
    | count1 == count2 = True
    | otherwise = False
equivI (Indifferent count1 _) (Indifferent count2 _)
    | count1 == count2 = True
    | otherwise = False
equivI _ _ = False

-- Infix equivalent of equiv
infixl 6 !=!
(!=!) :: TIndex -> TIndex -> Bool
(!=!) = equivI


