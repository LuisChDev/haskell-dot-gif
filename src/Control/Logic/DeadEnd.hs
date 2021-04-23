{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
module DeadEnd where

import Prelude as P hiding (head)

import qualified Data.Sequence as SQ

import Data.Matrix hiding (trace)
import Text.Parsec hiding (State)
import Text.Parsec.Text (Parser)
import Relude.Unsafe (read)
import Data.Text (pack, unpack)

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Query.DFS (components)
import Data.Graph.Inductive.PatriciaTree
import Data.List (delete)

import Relude.Unsafe (head)

type Streets = Matrix Bool -- si los componentes i,j están conectados o no
type Grafo = UGr

type Camino = (Node, Node)


-- | función para generar el grafo.
-- modifícala dependiendo del tipo de dato que prefieras.
parseInp :: Parser UGr -- Streets
parseInp = do
  (n, m) <- do
    ints <- (many1 digit) `sepBy` (char ' ')
    satisfy (== '\n')
    pure $ let [n, m] = take 2 ints in (n, m)
  edges <- ((many1 digit) `sepBy` (char ' ')) `sepBy` (char '\n')
  let
    num = read n
    lNodes = [(nd, ()) | nd <- [1..num]]
    conns = [(read x, read y) | [x, y] <- edges]
    lEdges = [(x, y, ()) | (x, y) <- conns]
    lEdges' = lEdges ++ [(y, x, ()) | (x, y) <- conns]
    in
    -- pure $ matrix num num (\tup -> tup `elem` conns)
    pure $ mkGraph lNodes lEdges'

{- Hay dos posibles resultados. Uno es que el grafo resultante
   sea un árbol. En cuyo caso, todos las calles son sin salida, pero únicamente
   es dado poner avisos de callejón sin salida en los nodos hoja, ya que todos
   los nodos interiores se pueden acceder desde al menos algún otro nodo
   adentro.

   En el segundo caso (y más general), hay ciclos dentro del grafo.
   La solución del problema consiste en hallar los ciclos y marcar los aristas
   de salida como callejones sin salida.
 -}

-- | Se obtienen los nodos que son parte de un ciclo.
-- se considera un ciclo una colección de al menos tres elementos x, y, z,
-- donde x sea alcanzable desde y, y desde z y z desde x.
-- cycles :: Grafo -> [Node]
-- cycles grf =
-- map fst . filter isCyclic .
loops :: Grafo -> [Node]
loops grf = filter anyLoop (nodes grf)
  where
    anyLoop :: Node -> Bool
    anyLoop nd = any (\x -> fst $ runState (isLoop grf nd x) [])
      (delete nd $ nodes grf)
    isLoop :: Grafo -> Node -> Node -> State [Node] Bool
    isLoop grf frm dst = do
      if frm == dst then trace "turning back" $ backtrip grf frm dst
        else isLoop' isLoop grf frm dst
    backtrip   _ frm dst | frm == dst = pure True
    backtrip grf frm dst = isLoop' backtrip grf frm dst
    isLoop' recr grf frm dst = do
      seen <- get
      if frm `elem` seen then pure False else do
        put (frm:seen)
        sen <- get
        trace (show sen) $ pure False
        anyM (recr grf dst) (neighbors grf frm)

-- | versión alternativa de búsqueda de ciclos, de forma pura.
areInLoop :: Grafo -> [Node]
areInLoop grf = filter inALoop (nodes grf)
  where
    inALoop :: Node -> Bool
    inALoop nde = any (isLoop SQ.empty nde) (delete nde $ nodes grf)
    isLoop seen frm dst
      | frm == dst && (length seen > 1) =
        backtrip (SQ.deleteAt (length seen - 1) seen) dst frm
    isLoop seen frm dst = isLoop' isLoop seen frm dst
    backtrip    _ frm dst | frm == dst = True
    backtrip seen frm dst = isLoop' backtrip seen frm dst
    isLoop'    _ seen frm   _ | frm `elem` seen = False
    isLoop' recr seen frm dst =
      any (\x -> recr (frm SQ.<| seen) x dst) (neighbors grf frm)

-- | otra versión del algoritmo.
inALoop :: Grafo -> [Node]
inALoop grf = nodes $ fst $ flip runState grf remove
  where
    remove = do
      curGrf <- get
      let newGrf = nfilter (\nde -> length (out curGrf nde) >= 2) curGrf
        in if newGrf == curGrf
        then pure newGrf
        else do
        put newGrf
        remove

-- | luego, recorremos la lista de grafos obtenida anteriormente, verificamos
-- si hay vecinos que NO estén en la lista, y anotamos el camino como un
-- callejón sin salida.
sinSalida :: Grafo -> [Node] -> [Camino]
sinSalida grf [] = [ (leaf, head $ suc grf leaf) | leaf <- nodes $ nfilter (\x -> length (suc grf x) == 1) grf]
sinSalida grf lops = join $ fmap (
  \nod -> [(nod, x) | x <- filter (\y -> not (elem y lops)) (neighbors grf nod)]) lops

-- | generamos el documento de salida a partir de la lista de caminos generados
-- con anterioridad.
printSinSal :: [Camino] -> String
printSinSal roads = (show $ length roads) <> "\n"
  <> (concatMap (\(x, y) -> (show x) <> " " <> (show y) <> "\n") roads)

main :: IO ()
main = do
  putTextLn "Bienvenido, por favor ingrese el nombre del programa"
  nameFile <- getLine
  content <- readFileText $ unpack nameFile
  case parse parseInp "" content of
    Left err -> fail $ "too bad! " <> show err
    Right matr -> let
      graphs = (flip subgraph matr) <$> (components matr)
      in mapM_ (\grph -> putStrLn $
                 printSinSal $ sinSalida grph $ inALoop grph) graphs
