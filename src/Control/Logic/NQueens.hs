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
  ++ [(z, w) | z <- [1..cl], w <- [1..cl],
      n <- [0..cl], x == z + n && y == w + n || x == z - n && y == w - n] -- diagonal
  ++ [(z, w) | z <- [1..cl], w <- [1..cl],
      z + w == x + y]                           -- anti-diagonal

-- | toma una reina y una lista de reinas, y retorna falso si la posición
--   de la reina es ilegal.
isValid :: Int -> Pos -> [Pos] -> Bool
isValid _ _ [] = True
isValid n q (p:pss) = if q `elem` (takePos n p)
  then False
  else isValid n q pss

-- | genera todas las posibles posiciones de una reina y elimina las
--   imposibles.
nQueen :: Int -> [Pos] -> [Pos]
nQueen n qns = do
  qnn <- [(x, y) | x <- [1..n], y <- [1..n]]
  guard $ isValid n qnn qns
  pure qnn


-- | crea todos los posibles tableros
tableros :: Int -> [[Pos]]
tableros n = do
  qn1 <- [(x, y) | x <- [1..n], y <- [1..n]]
  
  qn2 <- [(x, y) | x <- [1..n], y <- [1..n]]
  guard $ isValid n qn2 [qn1]
  qn3 <- [(x, y) | x <- [1..n], y <- [1..n]]
  guard $ isValid n qn3 [qn1, qn2]
  qn4 <- [(x, y) | x <- [1..n], y <- [1..n]]
  guard $ isValid n qn4 [qn1, qn2, qn3]
  -- qn5 <- [(x, y) | x <- [1..n], y <- [1..n]]
  -- guard $ isValid n qn5 [qn1, qn2, qn3, qn4]
  -- qn6 <- [(x, y) | x <- [1..n], y <- [1..n]]
  -- guard $ isValid n qn6 [qn1, qn2, qn3, qn4, qn5]
  -- qn7 <- [(x, y) | x <- [1..n], y <- [1..n]]
  -- guard $ isValid n qn7 [qn1, qn2, qn3, qn4, qn5, qn6]
  -- qn8 <- [(x, y) | x <- [1..n], y <- [1..n]]
  -- guard $ isValid n qn8 [qn1, qn2, qn3, qn4, qn5, qn6, qn7]
  pure [qn1, qn2, qn3, qn4] --, qn5, qn6, qn7, qn8]

-- | elimina las permutaciones de una lista.
rmPerm :: (Eq a) => [[a]] -> [[a]]
rmPerm [] = []
rmPerm (x:[]) = [x]
rmPerm (x:ys) = rmPerm $ x : (filter (`elem` permutations x) ys)
