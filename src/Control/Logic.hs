{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DerivingStrategies #-}

module Control.Logic
  ( solucion
  )
where

import qualified Text.Show                     as TS

{- En una tribu remota, los menores de edad sólo dicen la verdad, y los
mayores siempre alternan entre decir la verdad y mentir.

Un investigador se acerca a un grupo de adolescentes que bien podrían ser
mayores o menores de 18: Alicia, Beto y Carlos. Le pregunta a Carlos su edad.
Carlos responde, pero en su idioma. El investigador solicita a sus amigos,
que saben español, que lo traduzcan. Sus respuestas son:

Alicia: Carlos dijo "soy menor".
Beto: Carlos es mayor. Carlos mintió.

Es Carlos mayor o menor de 18 años? Qué edad dijo tener? Qué edades tienen
Alicia y Beto?
-}

-- | Representamos la edad mediante un tipo Suma
data Age = Mayor | Menor deriving stock (Eq, Show)

-- | Este tipo de dato representa una posible respuesta al problema.
data Answer = Answer
  { alicia :: Age,
    beto :: Age,
    carlos :: Age,
    carlosDice :: Age
  }

instance Show Answer where
  show Answer { alicia, beto, carlos, carlosDice } =
    "Alicia es "
      <> show alicia
      <> " de edad\n"
      <> "Beto es "
      <> show beto
      <> " de edad\n"
      <> "Carlos es "
      <> show carlos
      <> " de edad\n"
      <> "Carlos dice ser "
      <> show carlosDice
      <> "\n"

-- Ahora las reglas.
-- Cada regla lógica se puede representar como una función.

-- | (carlosDiceValido edadCarlosDice edadCarlos) retorna Falso si los valores
-- de verdad de la `afirmacion` de Carlos y la `realidad` llevarían a una
-- contradicción (i.e., que fuera menor de edad y afirmara ser mayor,
-- pues los menores nunca mienten).
carlosDiceValido :: Age -> Age -> Bool
carlosDiceValido Mayor Menor = False
carlosDiceValido _     _     = True

-- | (aliciaDiceValido edadAlicia carlosDijo)
-- En este caso, es imposible que Alicia sea menor de edad y Carlos haya
-- mentido. Todos los demás casos son posibles.
aliciaDiceValido :: Age -> Age -> Bool
aliciaDiceValido Menor Mayor = False
aliciaDiceValido _     _     = True

-- | (betoDiceValido edadBeto edadCarlos edadCarlosDice)
-- Si Beto es menor de edad, ambas afirmaciones deben ser verdaderas, por lo
-- que Carlos debe haber dicho ser menor siendo mayor.
-- Si Beto es mayor, tiene que haber mentido en alguno de los dos casos. El
-- primero es imposible, porque un menor no puede mentir. En el segundo,
-- da igual la edad de Carlos, porque pudo haber dicho la verdad.
-- todos los demás casos son imposibles.
betoDiceValido :: Age -> Age -> Age -> Bool
betoDiceValido eBeto eCarlos eCarDi = case (eBeto, eCarlos, eCarDi) of
  (Menor, Mayor, Menor) -> True
  (Mayor, _    , Menor) -> True
  _                     -> False

{- Para ejecutar el programa, se usa la mónada de lista.

   Esta mónada es equivalente en funcionalidad a una comprensión de lista: por
   cada variable generada, se multiplica el número de posibles valores
   resultantes en la lista final, por el número de valores posibles para la
   variable. El resultado es todos los posibles valores que puede asumir una
   solución al problema (i.e., el espacio de soluciones). El resto del ejercicio
   consiste en descartar las soluciones inválidas, de modo que se retorne la
   lista de soluciones correctas.

   este método (por "fuerza bruta") es ineficiente al generar todo el espacio de
   soluciones (si bien es cierto que la evaluación no-estricta puede reducir el
   número). La esencia del problema es una búsqueda en un espacio de soluciones
   y podría aventajarse del uso de heurísticas de búsqueda u otros métodos
   estadísticos modernos.
 -}
solucion :: [Answer]
solucion = do
  edadAlicia     <- [Mayor, Menor]
  edadBeto       <- [Mayor, Menor]
  edadCarlos     <- [Mayor, Menor]
  edadCarlosDice <- [Mayor, Menor]
  -- la expresión "guard a" toma un valor de verdad y cancela el resto
  -- del cálculo si es falso. Simplemente secuenciando todas las guardas
  -- podemos filtrar todas las situaciones imposibles, de modo que sólo
  -- queden las soluciones del problema.
  guard $ carlosDiceValido edadCarlosDice edadCarlos
  guard $ aliciaDiceValido edadAlicia edadCarlosDice
  guard $ betoDiceValido edadBeto edadCarlos edadCarlosDice
  -- se retorna la solución. Hay que tener en cuenta que si existe más de
  -- una, se retornan en lista.
  return $ Answer { alicia     = edadAlicia
                  , beto       = edadBeto
                  , carlos     = edadCarlos
                  , carlosDice = edadCarlosDice
                  }
