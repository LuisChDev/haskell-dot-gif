{- |
Copyright: (c) 2021 Luis Chavarriaga
SPDX-License-Identifier: MIT
Maintainer: Luis Chavarriaga <luischa123@gmail.com>

Este repositorio demuestra algunas técnicas para la animación usando Haskell y FRP
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE  Arrows #-}

module HaskellDotGif
       ( animate
       ) where

import FRP.Yampa
import Diagrams.Prelude
import Data.Colour.SRGB.Linear (rgb)
import Diagrams.Backend.Cairo.CmdLine

import Data.Fixed (mod')


animate :: IO ()
animate = do
  putTextLn $ "escoge un dibujo:\n \"1\" para rotación,\n" <>
    "\"2\" para órbita,\n" <>
    "\"3\" para arco iris"
  choice <- getLine
  case choice of
    "1" -> mainWith $ take 120 $ frames rotatin
    "2" -> mainWith $ take 120 $ frames orbit
    "3" -> mainWith $ take 120 $ frames rainbow


-- |
-- Las animaciones siempre están compuestas de listas de diagramas.
-- como las listas en Haskell son flojas, no hay pérdida de eficiencia
-- pues pueden generarse en la medida que se consumen para generar el gif.
frames :: SF () (Diagram B) -> [(Diagram B, Int)]
frames generator = zip (embed generator $ deltaEncode 1 $ repeat ()) (repeat 1)

-- |
-- los generadores tienen tipo SF () (Diagram B), esto es, son flechas (en el
-- sentido de teoría de categorías) de valor constante a una instancia de
-- Diagrama. Internamente, usan una flecha de tiempo para representar el
-- flujo de valores de tiempo y la relación con los valores de Diagrama.
rotatin :: SF () (Diagram B)
rotatin = proc () -> do
  t <- time -< ()
  returnA -< square 4
    # fc yellow
    # lw veryThick
    # lc orange
    # (rotate $ t @@ deg)  -- el cuadrado rota t grados

-- |
-- dado que la generación de diagramas se hace a partir de la composición
-- de elementos, es posible incorporar el elemento variable t de formas muy
-- sofisticadas.
orbit :: SF () (Diagram B)
orbit = proc () -> do
  t <- time -< ()
  returnA -< earth
    # translateX (6 * cos ((t*pi)/180))
    # translateY (6 * sin ((t*pi)/180))
    <> sun
  where
    sun = circle 4 # fc yellow
    earth = circle 0.5 # fc lightblue


-- |
-- no sólo es posible cambiar la ubicación de los objetos. Cualquier propiedad
-- que se puede configurar, se puede hacer variar en función del tiempo.
rainbow :: SF () (Diagram B)
rainbow = proc () -> do
  t <- time -< ()
  returnA -< mconcat [
    circle 1 # fc (uncu3 rgb (c t)),
    circle 2 # fc (uncu3 rgb (c (t + 1))),
    circle 3 # fc (uncu3 rgb (c (t + 2))),
    circle 4 # fc (uncu3 rgb (c (t + 3)))
                     ]
  where
    -- calcula el valor del componente de color
    c o = let th = ((o*pi)/45)
          in (cos th `mod'` 256,
              cos (th + 120) `mod'` 256,
              cos (th + 240) `mod'` 256)

    uncu3 f (x, y, z) = f x y z
