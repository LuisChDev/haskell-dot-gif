{-# LANGUAGE TypeApplications #-}
module Control.Parallel where

import GHC.Conc as C
import Data.List (partition)
import System.Random
import Relude.Unsafe ((!!))

-- -- -- --
-- demostración de paralelismo en funciones puras haciendo uso de par y pseq.

-- | quicksort ingenuo estándar en Haskell.
qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort lesser ++ x:(qsort greater)
  where
    (lesser, greater) = partition (<x) xs

-- Es evidente que ordenar las mitades inferior y superior de la lista
-- resultante no se va a ver afectada por la otra. Por tanto, tenemos una
-- oportunidad de paralelización.
--
-- la función `par' toma dos parámetros y retorna el segundo. Se diferencia de
-- const en que evalúa ambos parámetros al tiempo, creando un hilo nuevo para
-- ello. `pseq' toma dos atributos y se encarga de que el primero haya
-- terminado de evaluar antes de evaluar el siguiente. Podemos usar esto para
-- retornar la lista completa en cuanto se hayan terminado de evaluar las
-- sublistas.

qsort' :: (Ord a, NFData a) => [a] -> [a]
qsort' [] = []
qsort' (x:xs) = (force $ qsort' lesser `par` force $ qsort' greater)
  `pseq` qsort' lesser ++ x:(qsort' greater)
  where
    (lesser, greater) = partition (<x) xs

-- una segunda ronda de optimización reduce el número de hilos total a 8
-- terminando la paralelización a partir del tercer nivel de recursión.
qsort'' :: (Ord a, NFData a) => Int -> [a] -> [a]
qsort'' _ [] = []
qsort'' i lst@(x:xs)
  | i >= 2    = qsort lst
  | otherwise = (force $ qsort'' (i+1) lesser `par` force $ qsort'' (i+1) greater)
  `pseq` qsort'' (i+1) lesser ++ x:(qsort'' (i+1) greater)
  where
    (lesser, greater) = partition (<x) xs

-- la forma en Haskell de realizar concurrencia y paralelismo con efectos
-- es a través de mónadas. En particular, queremos demostrar el uso de la
-- llamada memoria transaccional.
--

type Semaphore = TVar Bool

newSem :: Bool -> IO Semaphore
newSem available = C.newTVarIO available

p :: Semaphore -> STM ()
p sem = do
  b <- readTVar sem
  if b
    then writeTVar sem False
    else retry

v :: Semaphore -> STM ()
v sem = writeTVar sem True

type Buffer a = TVar [a]

newBuffer :: IO (Buffer a)
newBuffer = C.newTVarIO []

putB :: Buffer a -> a -> STM ()
putB buffer item = do
  ls <- readTVar buffer
  writeTVar buffer (ls <> [item])

getB :: Buffer a -> STM a
getB buffer = do
  ls <- readTVar buffer
  case ls of
    [] -> retry
    (item:rest) -> do
      writeTVar buffer rest
      return item

randomDelay :: IO ()
randomDelay = do
  r <- randomRIO (100000, 500000)
  threadDelay r

philosopher :: Int -> Buffer Text -> Semaphore -> Semaphore -> IO ()
philosopher n out fork1 fork2 = do
  C.atomically $ putB out ("El filośofo " <> show n <> " Está pensando.")
  randomDelay

  C.atomically $ do
    p fork1
    p fork2

  C.atomically $ putB out ("El filósofo " <> show n <> " Está cenando.")
  randomDelay

  C.atomically $ do
    v fork1
    v fork2

  philosopher n out fork1 fork2

simulation :: Int -> IO ()
simulation n = do
  forks <- replicateM n (newSem True)
  outputBuffer <- newBuffer
  forM_ [0..n-1] $ \i ->
    forkIO (philosopher i outputBuffer
            (forks !! i)
            (forks !! ( (i+1) `mod` n) )
           )
  output outputBuffer

output :: Buffer Text -> IO ()
output buffer = do
  str <- C.atomically $ getB buffer
  putTextLn str
  output buffer

