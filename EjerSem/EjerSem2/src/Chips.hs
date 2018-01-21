{-
- Estructuras discretas 2017-1
- Clase que representa algunos chips
- de circuitos digitales
- Profesor: Dr. Favio Ezequiel Miranda Perea
- Ayudante: Victor Zamora Gutierrez
- Laboratorio: Fernando Abigail Galicia Mendoza
- Semanal 2.
- Galeana Araujo Emiliano 
-}

module Chips where

-- Importamos el modulo que contiene a la clase de las compuertas
import Compuertas

-- | Chip. Clase que representa la abstraccion
-- de los chips aplicados a cualquier tipo de datos.
class Compuerta a => Chip a where

  -- | ha. Metodo que representa el semi-sumador.
  ha :: Chip a => a -> a -> (a,a)

  -- | fa. Metodo que representa el sumador completo.
  fa :: Chip a => a -> a -> a -> (a,a)

  -- | mux4x1. Metodo que representa el multiplexor de 4 entradas y una salida.
  mux4x1 :: Chip a => a -> a -> a -> a -> (a,a) -> a

instance Chip Bool where
  --Metodo que representa la semisuma para el tipo Bool
  --	   (Suma     ,	Acarreo)	
  ha x y = (xor2 x y , and2 x y)

  --Metodo que representa la suma completa para el tipo Bool
  --		(Suma	,	Acarreo)
  fa c1 b1 b2 = ((xor2(xor2 b1 b2) c1),(or2(and2 b1 b2) (and2 c1 (xor2 b1 b2))))

  --Metodo que representa el mutiplexor de 4 entradas una salida para el tipo Bool
  mux4x1 e1 e2 e3 e4 (False, False) = e1
  mux4x1 e1 e2 e3 e4 (False, True) = e2
  mux4x1 e1 e2 e3 e4 (True, False) = e3
  mux4x1 e1 e2 e3 e4 (True, True) = e4
  
-- | toby. Funcion que representa el circuito minimo de el mecanismo de votacion.
toby :: Bool -> Bool -> Bool -> Bool -> Bool
toby t i1 i2 i3 = (or2 (or2(and2 t  i3)(and2 t i2)) (or2 (and2 t i1)(and2 (and2 i1 i2) i3)))

--Ejemplos

--Respuesta: (False,True)
ejemploC0 = ha True True

--Respuesta: (True,False)
ejemploC1 = ha False True

--Respuesta: (False,True)
ejemploC2 = fa False True True

--Respuesta: (True,True)
ejemploC3 = fa True True True

--Respuesta: False
ejemploC4 = mux4x1 True False False False (True,False)

--Respuesta: True
ejemploC5 = mux4x1 False False False True (True,True)

--Respuesta: True
ejemploC6 = toby True False False True

--Respuesta: False
ejemploC7 = toby True False False False
