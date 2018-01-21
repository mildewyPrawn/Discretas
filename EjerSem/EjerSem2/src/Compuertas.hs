{-
- Estructuras Discretas 2017-1
- Clase que representa las compuertas
- de circuitos digitales.
- Profesor: Dr. Favio Ezequiel Miranda Perea
- Ayudante: Victor Zamora Gutierrez
- Laboratorio: Fernando Abigail Galicia Mendoza
- Semanal 2.
- Galeana Araujo Emiliano 
-}

module Compuertas where

-- | Compuerta. Clase que representa la abstraccion
-- de las compuertas aplicadas a cualquier tipo de datos
class Compuerta a where

  -- | neg. Metodo que representa la compuerta NOT
  neg :: Compuerta a => a -> a

  -- | and2. Metodo que representa la compuarta AND
  and2 :: Compuerta a => a -> a -> a

  -- | or2. Metodo que representa la compuerta OR
  or2 :: Compuerta a => a -> a -> a

  -- | nand2. Metodo que representa la compuerta NAND
  nand2 :: Compuerta a => a -> a -> a

  -- | nor2. Metodo que representa la compuerta NOR
  nor2 :: Compuerta a => a -> a -> a

  -- | xor2. Metodo que representa la compuerta XOR
  xor2 :: Compuerta a => a -> a -> a

  -- | xnor2. Metodo que representa la compuerta XNOR
  xnor2 :: Compuerta a => a -> a -> a

instance Compuerta Bool where
  --Metodo que representa la compuerta de negacion para el tipo Bool
  neg False = True
  neg True = False
  --Metodo que representa la compuerta de la conjuncion para el tipo Bool
  and2 True True = True
  and2 True False = False
  and2 False True = False
  and2 False False = False
  --Metodo que representa la compuerta de la disyuncion para el tipo Bool
  or2 True True = True 
  or2 True False = True
  or2 False True = True
  or2 False False = False

  --Metodo que representa la compuerta de la conjuncion negada para el tipo Bool
  nand2  True True = False
  nand2  True False = True 
  nand2  False True = True 
  nand2   False False = True  

  --Metodo que representa la compuerta de la disyuncion negada para el tipo Bool
  nor2 True True = False
  nor2 True False = False
  nor2 False True = False
  nor2 False False = True

  --Metodo que representa la compuerta de la disyunción exclusiva para el tipo Bool
  xor2  True True = False
  xor2  True False = True 
  xor2  False True = True
  xor2  False False = False

  --Metodo que representa la compuerta de la disyunción negada exclusiva para el tipo Bool
  xnor2 True True = True 
  xnor2 True False = False
  xnor2 False True = False
  xnor2 False False = True

--Ejemplos:

--Respuesta: False
ejemplo0 = neg True

--Respuesta: True
ejemplo1 = and2 True True

--Respuesta: False
ejemplo2 = or2 False False

--Respuesta: True
ejemplo3 = nand2 True False

--Respuesta: False
ejemplo4 = nor2 False True

--Respuesta: True
ejemplo5 = xor2 False True 

--Respuesta: False
ejemplo6 = xnor2 False True
