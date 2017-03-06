{-|
Module      : Tensor
Description : Tensor is a generic datatype to deal with linear algebra. May contain co- and contravariant indices as well. 
Copyright   : (c) Artur M. Brodzki, 2017
License     : 3-clause BSD
Maintainer  : artur.brodzki@gmail.com
Stability   : experimental
Portability : Windows/POSIX

-}

module Multilinear.Tensor (
  T(..), el, ind
) where

{-| Tensor datatype -}
data T a where
    T :: String -> String -> ([Int] -> [Int] -> a) -> T a
  
{-| Tensor component -}
el :: T a -> ([Int] -> [Int] -> a)
el (T _ _ f) = f

{-| Tensors can be added and multiplicated -}
instance Num a => Num (T a) where
    (T us1 ds1 f1) + (T us2 ds2 f2) 
        | us1 == us2 && ds1 == ds2 = 
            T us1 ds1 (\ul dl -> f1 ul dl + f2 ul dl) 
        | otherwise = error "Tensors not compatible!"
    
    (T us1 ds1 f1) - (T us2 ds2 f2) 
        | us1 == us2 && ds1 == ds2 = T us1 ds1 (\ul dl -> f1 ul dl - f2 ul dl) 
        | otherwise = error "Tensors not compatible!"

    (T us1 ds1 f1) * (T us2 ds2 f2) = 
        T (us1 ++ us2) (ds1 ++ ds2) (
            \ul dl -> f1 (take (length us1) ul) (take (length ds1) dl) * f2 (drop (length us1) ul) (drop (length ds1) dl)
        )
    
    abs (T us ds f) = T us ds (\ul dl -> abs (f ul dl))

    signum (T us ds f) = T us ds (\ul dl -> signum (f ul dl))

{-| Returns tensor indices -}
ind :: T a -> (String,String)
ind (T us ds _) = (us,ds)