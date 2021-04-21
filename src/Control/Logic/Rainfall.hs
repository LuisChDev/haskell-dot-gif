{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ViewPatterns #-}

module Control.Logic.Rainfall
  ( main
  )
where

import Refined
  ( And,
    Even,
    FromTo,
    RefineException,
    Refined,
    SizeLessThan,
    refine,
    refineFail,
    unrefine,
  )
import Relude.Unsafe (read)
  -- sólo usamos esto dentro de parsers donde sabemos
  -- que no va a arrojar excepciones
import Text.ParserCombinators.ReadP (ReadP, many1, readP_to_S, satisfy)
import System.FilePath
import qualified Text.Show as TS
import qualified Data.Text as T


{- Esta solución usa "tipos de refinamiento", que extienden un tipo básico
   con un predicado. Se espera que todo valor que habite el tipo cumpla el
   predicado.
 -}

-- | el tablero de damas se representa como una secuencia de
-- 32 casillas, más un número de piezas (y su respectiva posición)
-- que no pueden pasar de esa cantidad.
newtype Tablero = Tab {unTab :: Refined (SizeLessThan 24) (Seq (Pieza, Pos))}

instance Show Tablero where
  show = T.unpack . showText

-- las cadenas en Haskell son malísimas
showText :: Tablero -> Text
showText (unrefine . unTab -> tab) =
  let showPiece pieza = case pieza of
        Pieza Blancas -> 'w'
        Pieza Negras -> 'b'
        Reina Blancas -> 'W'
        Reina Negras -> 'B'
   in T.intercalate "\n" $
        T.chunksOf 8 $
          foldl'
            ( \str (piez, unrefine -> x) ->
                let (prev, T.drop 1 -> next) = T.splitAt (x -1) str
                 in prev <> T.cons (showPiece piez) next
            )
            (T.replicate 4 $ "-.-.-.-." <> ".-.-.-.-")
            $ tab

-- | las piezas pueden ser normales o "reinas".
data Pieza = Pieza Jugador | Reina Jugador

-- | el jugador puede ser blanco o negro.
data Jugador = Blancas | Negras

-- | las jugadas son bien movimientos o capturas.
data Jugada = Mov Pos Pos | Cap Pos Pos deriving stock (Show)

type Pos = Refined (And Even (FromTo 1 64)) Int

-- | calcula la posición en medio de un movimiento de captura.
mitad :: Pos -> Pos -> Either RefineException Pos
mitad (unrefine -> x) (unrefine -> y) =
  let op = if x < y then (-) else (+)
   in refine (y `op` 9)

{- es posible resolver este problema como uno de resolución de restricciones.
   cada jugada de las dadas por el usuario constituye una restricción, i.e., si
   blancas mueve 22x13, es necesario que en la casilla 13 esa jugada haya una
   pieza o reina negra, no puede estar vacía o contener sus equivalentes
   blancas. Si las negras mueven 11-20, quiere decir que no había ninguna
   posibilidad de capturar ese mismo turno, etc.

   Este programa genera los posibles escenarios y los filtra a través de las
   restricciones que imponen las jugadas dadas (que son convertidas en
   funciones).
 -}

-- | restricción generada por una movida normal.
-- Mov (init, end)
-- 1. la casilla de inicio debe contener una pieza del color correcto.
--    esto implica que debió iniciar así o una jugada anterior debió terminar
--    aquí.
-- 2. la casilla final debe estar vacía. requerimientos similares a lo
--    anterior.
-- 3. no debe haber ninguna posible jugada de captura para el jugador.
-- 4. si el movimiento es en sentido contrario al natural para el color,
--    se debe asumir que lo está realizando una pieza reina, que empezó la
--    partida o se convirtió en plena partida.
-- Restricción generada por una captura.
-- Cap (init, end)
-- 1. la casilla de en medio debe estar ocupada por una pieza del otro color.
-- 2. se cumplen las mismas otras restricciones que en el caso del movimiento
--    (sobre las casillas inicial y final).
mkRestr :: Jugada -> Tablero -> Bool
mkRestr (Mov (unrefine -> x) (unrefine -> y))
  | otherwise = const False
mkRestr (Cap (unrefine -> x) (unrefine -> y))
  | otherwise = const False





-- | código dedicado a parsear la cadena de entrada.
parse :: String -> Maybe (Jugador, [Jugada])
parse str = do
  posibleParse <- listToMaybe $ flip readP_to_S str $ do
    (jug, num) <- firstLn
    jugs <- many1 parseJug
    pure (jug, join jugs)
  pure $ fst posibleParse

-- | parsea la primera línea, obteniendo el color inicial y el # de jugadas.
firstLn :: ReadP (Jugador, Int)
firstLn = do
  player <- satisfy (flip elem ['W', 'B'])
  satisfy (== ' ')
  count <- many1 digit
  satisfy (== '\n')
  pure
    ( case player of 'W' -> Blancas; 'B' -> Negras,
      read count -- nunca va a haber excepción,
      -- porque el parser se encarga
      -- de seleccionar dígitos
    )

-- | parsea una línea, retornando una o más jugadas.
parseJug :: ReadP [Jugada]
parseJug = do
  initNum <- many1 digit
  inum <- refineFail (read initNum)
  movs <- many1 parseMov
  satisfy (== '\n')
  pure $ snd $ foldl' (\(prev, jugs) (next, move)
                       -> (next, jugs <> [move prev])) (inum, []) movs

-- | parsea un solo movimiento. Retorna la posición final y una función
-- que recibe la posición inicial y retorna la jugada.
parseMov :: ReadP (Pos, Pos -> Jugada)
parseMov = do
  op <- satisfy (flip elem ['-', 'x'])
  num <- many1 digit
  pos <- refineFail (read num) -- se captura fuera del parser
  let func = (case op of
                '-' -> flip Mov pos
                'x' -> flip Cap pos)
  pure (pos, func)

digit :: ReadP Char
digit = satisfy (\char -> char >= '0' && char <= '9')

-- -- --
-- rutina principal.
main :: IO ()
main = do
  putText "Bienvenido al programa. Favor ingrese el nombre del archivo."
  filepath <- getLine
  if (isValid . T.unpack) filepath
    then parseAndRun $ T.unpack filepath
    else do
    putText $ "Archivo no encontrado. Favor ingrese la dirección de "
      <> "un archivo válido."
    main

parseAndRun :: FilePath -> IO ()
parseAndRun flp = do
  undefined
