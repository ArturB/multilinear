module Main where

import           Control.Exception.Base
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Class
import           Multilinear.Class          as Multilinear
import           Multilinear.Generic
import qualified Multilinear.Matrix         as Matrix
import qualified Multilinear.Tensor         as Tensor
import qualified Multilinear.Vector         as Vector

-- PARAMETRY SKRYPTU
fi     = signum  -- funkcja aktywacji perceptronu
layers = 10      -- liczba warstw perceptronu

mlp_input         = "test/data/mlp_input.csv"          -- dane uczące dla perceptronu
mlp_expected      = "test/data/mlp_expected.csv"       -- dane oczekiwane dla percepttronu
mlp_classify      = "test/data/mlp_classify.csv"       -- dane do klasyfikacji na nauczonym perceptronie
mlp_output        = "test/data/mlp_output.csv"         -- wyjście perceptronu
hopfield_input    = "test/data/hopfield_input.csv"     -- wzorce do zpamiętania dla sieci Hopfielda
hopfield_classify = "test/data/hopfield_classify.csv"  -- dane do klasyfikacji dla sieci Hopfielda
hopfield_output   = "test/data/hopfield_output.csv"    -- wyjście sieci Hopfielda


-- PERCEPTRON WIELOWARSTWOWY
perceptron :: Int                    -- ns:  liczba neuronów w warstwie
           -> Int                    -- ks:  liczba warstw 
           -> Int                    -- ps:  liczba wektorów uczących
           -> Int                    -- cs:  liczba wektorów do klasyfikacji
           -> (Int -> Tensor Double) -- x t: wejścia uczące w funkcji czasu
           -> (Int -> Tensor Double) -- e t: wyjścia oczekiwane w funkcji czasu
           -> (Int -> Tensor Double) -- c t: dane do klasyfikacji w funkcji czasu
           -> Tensor Double          -- Tensor ("i","t"): zaklasyfikowane dane

perceptron ns ks ps cs x e c =
  let -- wagi startowe
      zero = Tensor.const ("ki",[ks,ns]) ("j",[ns]) 0
      -- wagi w następnym kroku uczącym
      nextWeights w x e =
        let ygen [k] [] = -- tensor wyjść
              if k == 0 then x $| ("j","") 
              else fi <$> w $$| ("k",[k - 1]) $| ("i","j") * ygen [k-1] [] $| ("j","")
            y = Tensor.generate ("k",[ks + 1]) ("",[]) $ \[k] [] -> ygen [k] []
            -- tensor wejścia-wyjścia omega
            om = Tensor.generate ("k",[ks]) ("",[]) $ 
              \[k] [] -> ygen [k + 1] [] $| ("i","") * ygen [k] [] $| ("j","") \/ "j"
            incWgen [k] [] = -- inkrementacyjna propagacja wsteczna
              if k == ks - 1 then x $| ("j","") \/ "j" * (y $$| ("k",[ks-1]) $| ("i","") - e $| ("i",""))
              else Multilinear.transpose (w $$| ("k",[k+1])) $| ("i","b") * 
                   incWgen [k+1] [] $| ("b","c") * 
                   om $$| ("k",[k]) $| ("c","j")
            incW = Tensor.generate ("k",[ks]) ("",[]) $ \[k] [] -> incWgen [k] []
        in  w $| ("ki","j") + incW $| ("ki","j")
      xl = take 2 $ x <$> [0 .. ps - 1]
      el = take 2 $ e <$> [0 .. ps - 1]
      -- uczenie sieci
      learnedNetwork = foldr (\(x,e) w -> nextWeights w x e) zero $ zip xl el
      -- praca nauczonej sieci
      out t = fi <$> learnedNetwork $$| ("k",[ks-1]) $| ("i","j") * c t $| ("j","")
  in  Tensor.generate ("",[]) ("t",[cs]) $ \[] [t] -> out t

-- SIEĆ HOPFIELDA
hopfield :: Int        -- ns: liczba neuronów w sieci
        -> Int         -- ps: liczba wzorców do zapamiętania
        -> Int         -- cs: liczba wzorców do klasyfikacji
        -> Tensor Int  --  x: macierz wektorów do zapamiętania
        -> Tensor Int  --  c: macierz wektorów do sklasyfikowania
        -> Tensor Int  --  macierz sklafyfikowanych wektorów
hopfield ns ps cs x c = 
  let -- 1 - deltaKroneckera
      delta = Matrix.fromIndices "ij" ns ns $ \i j -> if i == j then 0 else 1
      -- wagi sieci ze wzorców
      w = delta * x $| ("i","t") * (x $| ("j","t") \/ "j") * Vector.const "t" ps 1
      -- wyjście sieci: sieć działa rekurencyjnie aż do osiągnięcia stanu stabilnego
      y inp =
        let out = (\x -> if x > 0 then 1 else 0) <$> w $| ("i","j") * inp $| ("j","") 
        in  if out $| ("i","") == inp $| ("i","") then out else y out
      -- klasyfikacja zadanych wektorów
  in  Tensor.generate ("",[]) ("t",[cs]) $ \[] [t] -> y ((c $| ("i","t")) $$| ("t",[t]))

-- OPERACJE WEJŚCIA/WYJŚCIA
prog :: EitherT SomeException IO ()
prog = do
  -- wczytywanie danych
  mlpInput <- Matrix.fromCSV "tj" mlp_input ';'
  mlpExp   <- Matrix.fromCSV "tj" mlp_expected ';'
  mlpClas  <- Matrix.fromCSV "tj" mlp_classify ';'
  hopInput <- Matrix.fromCSV "tj" hopfield_input ';'
  hopClas  <- Matrix.fromCSV "tj" hopfield_classify ';'
  let mx t = Multilinear.transpose $ mlpInput $$| ("t",[t])
  let me t = Multilinear.transpose $ mlpExp $$| ("t",[t])
  let mc t = Multilinear.transpose $ mlpClas $$| ("t",[t])
  let hx = Multilinear.transpose $ hopInput $| ("it",[])
  let hc = Multilinear.transpose $ hopClas $| ("it",[])
  let (ns_mlp,ps_mlp,cs_mlp,ns_hop,ps_hop,cs_hop) = 
         (mlpInput `size` "j", mlpExp `size` "t", mlpClas `size` "t", 
         hopInput `size` "j", hopInput `size` "t", hopClas `size` "t")
  -- perceptron
  let mlp_net = perceptron ns_mlp layers ps_mlp cs_mlp mx me mc
  smlp <- lift $ Matrix.toCSV mlp_net mlp_output ';'
  -- hopfield
  let hop_net = hopfield ns_hop ps_hop cs_hop hx hc
  shop <- lift $ Matrix.toCSV hop_net hopfield_output ';'
  lift $ putStrLn $ "Perceptron: " ++ show smlp ++ " vectors saved to '" ++ mlp_output ++ "'."
  lift $ putStrLn $ "Hopfield: " ++ show shop ++ " vectors saved to '" ++ hopfield_output ++ "'."
  return ()

-- ENTRY POINT
main :: IO (Either SomeException ())
main = runEitherT prog
  
