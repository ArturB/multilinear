{-|
Module      : Index
Description : Implements tensor index.
Copyright   : (c) Artur M. Brodzki, 2017
License     : 3-clause BSD
Maintainer  : artur.brodzki@gmail.com
Stability   : experimental
Portability : Windows/POSIX

-}

{-# LANGUAGE Strict #-}

module Multilinear.Index (
    TIndex(..),
    equivI, (!=!),
) where

import           Data.Binary

{-| TENSOR INDEX -}
data TIndex i =
    Covariant {
        indexCount :: i,
        indexName  :: String
    } |
    Contravariant {
        indexCount :: i,
        indexName  :: String
    } |
    Indifferent {
        indexCount :: i,
        indexName  :: String
    }
    deriving Eq

{-| Serialization -}
instance Binary i => Binary (TIndex i) where
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

{-| Show instance of TIndex -}
instance Show i => Show (TIndex i) where
    show (Covariant c n)     = "[" ++ n ++ ":" ++ show c ++ "]"
    show (Contravariant c n) = "<" ++ n ++ ":" ++ show c ++ ">"
    show (Indifferent c n)   = "(" ++ n ++ ":" ++ show c ++ ")"

{-| Returns true if two indices are quivalent, i.e. differs only by name -}
equivI :: Eq i => TIndex i -> TIndex i -> Bool
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

{-| Infix operator equivalent of equiv -}
infixl 6 !=!
(!=!) :: Eq i => TIndex i -> TIndex i -> Bool
(!=!) = equivI


