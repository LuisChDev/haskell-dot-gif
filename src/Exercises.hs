{-# LANGUAGE ExplicitForAll #-}

-- Este módulo requiere la instalación de las bibliotecas
-- "mtl" y "vector" de Hackage.

module Exercises where

import Prelude as P

import Control.Monad.ST.Strict (runST)
import Data.List  as DL
import Data.Vector  as V
import Data.Vector.Mutable as MV
import Text.Read (readMaybe)
import Data.Text (unpack)
import Prelude hiding (length, null, read)
import Control.Monad (when)
import Data.Foldable (foldlM)

-- |
-- función recursiva de cola que calcula el largo de una lista
listLen :: [a] -> Int
listLen [] = 0
listLen xs = listLen' 0 xs
  where
    listLen' acc [] = acc
    listLen' acc (_ : ys) = listLen' (acc + 1) ys

-- |
-- función que calcula el máximo y el mínimo de una lista de enteros.
-- para propósitos demostrativos, se implementa de forma genérica,
-- para todos los tipos que definan operaciones de comparación.
maxMinL :: forall a. (Ord a) => [a] -> Maybe (a, a)
maxMinL [] = Nothing
maxMinL (x : []) = Just (x, x)
maxMinL xs = maxMinL' Nothing xs
  where
    maxMinL' :: (Ord a) => Maybe (a, a) -> [a] -> Maybe (a, a)
    maxMinL' acc [] = acc
    maxMinL' Nothing (y : ys) = maxMinL' (Just (y, y)) ys
    maxMinL' acc@(Just (z, w)) (y : ys)
      | y > z = maxMinL' (Just (y, w)) ys
      | y < w = maxMinL' (Just (z, y)) ys
      | otherwise = maxMinL' acc ys

-- |
-- lee los números ingresados por el usuario hasta encontrar un cero,
-- luego los retorna en una lista.
takeNums :: IO [Int]
takeNums = takeNums' []
  where
    takeNums' :: [Int] -> IO [Int]
    takeNums' xs = do
      putStrLn "Escriba un número."
      inp <- getLine
      case readMaybe (unpack inp) of
        Nothing -> do
          putStrLn "número inválido. Por favor ingrese un número."
          takeNums' xs
        Just num -> case num of
          0 -> return (0 : xs)
          x -> takeNums' (x : xs)

-- |
-- Quicksort, implementado en Haskell.
-- Esta primera versión es ineficiente, pero extremadamente concisa.
qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x : xs) = qsort lesser P.++ [x] P.++ qsort greater
  where
    (lesser, greater) = DL.partition (\y -> y < x) xs

-- Esta implementación  no cumple la propiedad más deseable del Quicksort, que
-- es no asignar más memoria de la ocupada originalmente por la lista.
-- esto no es posible de forma explícita en Haskell, debido a que, al menos
-- de jure, todo es inmutable, por tanto no es posible modificar variables (no
-- son realmente "variables", sino constantes).
-- Sin embargo, el compilador GHC suele ser capaz de optimizar este código,
-- especialmente si se escribe de una forma que lo facilite.
-- Para ello podemos usar la mónada ST, que describe una computación
-- que puede leer de y modificar un valor de estado.

-- | segunda implementación de Quicksort, mediante la mónada Estado.
-- esto es esencialmente una demostración de programación imperativa
-- en Haskell.
qsortS :: (Ord a) => [a] -> [a]
qsortS inp = V.toList $ runST $ qsortS' $ V.fromList inp
  where
    qsortS' vec = do
      mut <- thaw vec
      qsortS'' 1 (V.length vec - 1) mut
      freeze mut

    qsortS'' left right vec = when (right > left) $ do
      newpiv <- divide (left + ((right - left) `div` 2)) left right vec
      qsortS'' left (newpiv - 1) vec
      qsortS'' (newpiv + 1) right vec

    divide piv left right vec = do
      pivVal <- MV.read vec piv
      MV.swap vec piv right
      storeIndex <- foreachWith [left..right-1] left (
        \i storeindex -> do
          val <- MV.read vec i
          if (val <= pivVal)
            then do
            MV.swap vec i storeindex
            return (storeindex + 1)
            else return storeindex)
      MV.swap vec storeIndex right
      return storeIndex

    foreachWith xs v f = foldlM (flip f) v xs

-- | esta función toma un número n y retorna todos los triples pitagóricos
-- tales que 1 <= x <= y <= z <= n.
pytTriples :: Int -> [(Int, Int, Int)]
pytTriples n = [(x, y, z) |
                 x <- [1..n],
                 y <- [1..n],
                 z <- [1..n],
                 x < y,
                 y < z,
                 x*x + y*y == z*z]

-- | toma un número n como parámetro y retorna una función de incremento n.
sumaN :: Int -> (Int -> Int)
sumaN n = (+n)


main :: IO ()
main = do
  putStrLn "Vamos a ingresar unos números."
  nums <- takeNums
  putStrLn $ "los números son: " P.++ show nums
