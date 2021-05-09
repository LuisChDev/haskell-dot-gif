module Control.Logic.Bridges () where

-- import Text.ParserCombinators.ReadP
import Text.Parsec as P
import Text.Parsec.Char
import Text.Parsec.Text (Parser)
import Relude.Unsafe (read)

-- -- -- --
-- tipos.

type Coord = (Int, Int)


-- -- -- --
-- funciones de parsing.

-- | parsea la primera línea de la entrada.
firstLn :: Parser (Int, Int, Int, Int)
firstLn = do
  ints <- enteros 4
  case length ints of
    4 -> pure $ let [a, b, c, d] = take 4 ints
      in (a, b, c, d)
    _ -> fail "número errado de parámetros"

-- | parsea una línea de entrada indicando la coordenada
-- del terreno.
coordLn :: Parser (Int, Int)
coordLn = do
  ints <- enteros 2
  case length ints of
    2 -> pure $ let [a, b] = take 2 ints
      in (a, b)
    _ -> fail "número errado de parámetros"

-- | parsea un número de enteros separados por espacios.
enteros :: Int -> Parser [Int]
enteros c = count c $ do
  int <- many1 digit
  satisfy (== ' ')
  pure $ read int


-- -- -- --
-- lógica.

{- la altura del puente es fija. La forma del puente puede variar
de acuerdo al número y posicionamiento de sus pilares. La restricción
que se impone entonces tiene que ver con la altura de los arcos.

La altura de un arco en función de la distancia del centro del mismo
hacia los bordes (-r el izquierdo, r el derecho) es de
y = r (sin (acos (x/r))). La altura del terreno dados dos puntos
(x1, y1) y (x2, y2) es la línea recta que los conecta. 

El objetivo del programa es hallar el mínimo precio.
 -}
