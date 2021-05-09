{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds #-}
module Control.Logic.NQueens where

-- | Cada posición se puede representar como un par de enteros entre 1 y 8.
type Pos = (Int, Int)

-- | Calcula las posiciones que una reina del ajedrez puede capturar,
--   de acuerdo a su posición en el tablero.
takePos :: Int -> Pos -> [Pos]
takePos cl (x, y) = hashNub $
     [(x, z) | z <- [1..cl]]
  ++ [(z, y) | z <- [1..cl]]                    -- columnas
  ++ [(z, w) | z <- [1..cl], w <- [1..cl],      -- diagonal
      n <- [0..cl], x == z + n && y == w + n
       || x == z - n && y == w - n]
  ++ [(z, w) | z <- [1..cl], w <- [1..cl],      -- anti-diagonal
      z + w == x + y]

-- | toma una reina y una lista de reinas, y retorna falso si la posición
--   de la reina es ilegal.
isValid :: Int -> Pos -> [Pos] -> Bool
isValid _ _ [] = True
isValid n q (p:pss) = if q `elem` (takePos n p)
  then False
  else isValid n q pss

-- -- | genera todas las posibles posiciones de una reina y elimina las
-- --   imposibles.
-- nQueen :: Int -> [Pos] -> [Pos]
-- nQueen n qns = do
--   qnn <- [(x, y) | x <- [1..n], y <- [1..n]]
--   guard $ isValid n qnn qns
--   pure qnn

-- | crea todos los posibles tableros
tableros :: Int -> [[Pos]]
tableros n = tableros' 1 []
  where
    tableros' :: Int -> [Pos] -> [[Pos]]
    tableros' m qns = do
      qn <- [(m, y) | y <- [1 .. n]]
      guard $ isValid n qn qns
      if length qns == (n - 1)
        then pure $ qns ++ [qn]
        else tableros' (m + 1) $ qns ++ [qn]
