{-|
Module      : Tensor
Description : Tensor is a generic datatype to deal with linear algebra. May contain co- and contravariant indices as well. 
Copyright   : (c) Artur M. Brodzki, 2017
License     : 3-clause BSD
Maintainer  : artur.brodzki@gmail.com
Stability   : experimental
Portability : Windows/POSIX

-}

{-# LANGUAGE Strict, GADTs #-}

module Multilinear.Tensor (
  T(..), el, upIndices, lowIndices
) where

{-| Tensor datatype -}
data T a i where
    T :: Integral i => (String,[i]) -> (String,[i]) -> ([i] -> [i] -> a) -> T a i

{-| Tensors can be added, subtracted and multiplicated -}
instance (Num a, Integral i) => Num (T a i) where
    {-| Tensors with same indices can be added elem by elem -}
    (T us1 ds1 f1) + (T us2 ds2 f2) 
        | us1 == us2 && ds1 == ds2 = 
            T us1 ds1 (\ul dl -> f1 ul dl + f2 ul dl) 
        | otherwise = error incompatibleIndices
    
    {-| Tensors with same indices can be subtracted elem by elem -}
    (T us1 ds1 f1) - (T us2 ds2 f2) 
        | us1 == us2 && ds1 == ds2 = T us1 ds1 (\ul dl -> f1 ul dl - f2 ul dl) 
        | otherwise = error incompatibleIndices
    
    {-| Tensor product satisfying Einstein summation convention -}
    --(T [] [d] f1) * (T [u] [] f2) = T [] [] 
    (T (us1,usize1) (ds1,dsize1) f1) * (T (us2,usize2) (ds2,dsize2) f2) = 
        T (us1 ++ us2, usize1 ++ usize2) (ds1 ++ ds2, dsize1 ++ dsize2) (
            \ul dl -> f1 (take (length us1) ul) (take (length ds1) dl) * f2 (drop (length us1) ul) (drop (length ds1) dl)
        )
    
    {-| Scalar tensor from integral number -}
    fromInteger i = T ([],[]) ([],[]) (\_ _ -> fromInteger i)

    {-| Calculate absolute value elem by elem -}
    abs (T us ds f) = T us ds (\ul dl -> abs (f ul dl))

    {-| Calculate sign of tensor elements -}
    signum (T us ds f) = T us ds (\ul dl -> signum (f ul dl))

{-| Returns tensor upper indices -}
upIndices :: T a i -> [(Char,i)]
upIndices (T (us,usize) _ _) = zip us usize

{-| Return tensor lower indices -}
lowIndices :: T a i -> [(Char,i)]
lowIndices (T _ (ds, dsize) _) = zip ds dsize

{-| Get tensor element -}
el :: T a i -> ([i] -> [i] -> a)
el (T _ _ f) = f

incompatibleIndices :: String
incompatibleIndices = "Incompatible tensor indices!"