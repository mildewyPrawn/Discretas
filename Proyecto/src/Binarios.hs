{-
- Estructuras discretas 2017-1
- Profesor: Favio E. Miranda Perea
- Ayudante: Victor Zamora Gutierrez
- Laboratorio: Fernando A. Galicia Mendoza
- Proyecto 8 
- Integrantes:
- Galeana Araujo Emiliano	314032324	galeanaara@ciencias.unam.mx
- Márquez Castillo Irene	314318424	irene_marqz97@ciencias.unam.mx
-}

module Binarios where

{--
Define el tipo de datos que represente a BinPos, nombralo BinPos y
crea la instancia de la clase Show para mostrar los números binarios en
la notación usual.
--}

data BinPos  =  U | Cero (BinPos) | Uno (BinPos) deriving (Show)

-- | sucesor. Función que devuelve el sucesor de un elemento de BinarioPos.

sucesor :: BinPos -> BinPos 
-- sucesor de 1 = 10 representado como Cero x donde x es 1.
sucesor U = Cero U 
--sucesor de x0 donde x es una cadena de binarios.
sucesor (Cero x) = Uno x
--sucesor de x1 donde x es una cadena de binarios.
sucesor (Uno x) = Cero (sucesor x)  

-- | suma. Función recursiva que devuelve la suma de dos elementos de BinarioPos.

suma :: BinPos -> BinPos -> BinPos 
suma U U = sucesor U
suma U (Cero x) = sucesor (Cero x)
suma U (Uno x)  = sucesor (Uno x)
suma (Cero x) U = sucesor (Cero x)
suma (Cero x) (Uno y) = Uno (suma x y)
suma (Cero x)(Cero y) = Cero (suma x y)
suma (Uno x)(Cero y) = Uno (suma x y)
suma (Uno x) U  = sucesor (Uno x) 
suma (Uno x)(Uno y) = Cero (suma U (suma x y))

-- | longAux. Función auxiliar recursiva que devuelve la longitud de una cadena de BinarioPos.
{--	Esta función es por que si tenemos un número BinarioPos más grande que otro y como es
	resta en positivos, restar un número más grande a otro, nos devuelve la base.
--}
longAux :: BinPos -> Int
longAux U = 1 
longAux (Cero x) = 1 + longAux x
longAux (Uno x) = 1 + longAux x

-- | resta. Función recursiva que devuelve la resta positiva de dos elemtos de BinarioPos.

resta :: BinPos -> BinPos -> BinPos
resta U U = U
resta U (Cero x) = U
resta U (Uno x)  = U
resta (Cero x) U = Uno (resta x U) 
resta (Cero x) (Uno y) = if longAux y >= longAux x then U else Uno (resta x y)
resta (Cero x)(Cero y) = if longAux y >= longAux x then U else Cero (resta x y)
resta (Uno x)(Cero y) = if longAux y >= longAux x then U else Uno (resta x y) 
resta (Uno x) U  = (Cero x) 
resta (Uno x)(Uno y) = if longAux y >= longAux x then U else Cero (resta x y) 

-- | producto. Función recursiva que devuelve el producto de dos elementos de BinarioPos.

producto :: BinPos -> BinPos -> BinPos
producto U U = U
producto U (Cero x) = (Cero x)
producto U (Uno x)  = (Uno x)
producto (Cero x) U = (Cero x)
producto (Cero x) (Uno y) = suma (Cero x) ((Cero(Cero( producto x y)))) --error "Implementar"
producto (Cero x)(Cero y) = ((Cero(Cero (producto x y))))
producto (Uno x)(Cero y) = suma (Cero x) ((Cero(Cero( producto x y)))) --error "Implementar"
producto (Uno x) U  = (Uno x)
producto (Uno x)(Uno y) =suma  (producto U (Uno x)) (Cero (producto y (Uno x)))

-- | binAux. Función recursiva que recive un BinarioPos y un entero y devuelve el número entero 
--	que es su representación.

binAux :: BinPos -> Int -> Int
binAux b y = case b of 
	U -> 2^y 
	(Cero x) -> (binAux x (y+1)) 
	(Uno x) ->  (binAux x (y+1)) + 2^y

-- | binPosToInt. Función recursiva que dado un elemento de BinarioPos devuelve el número natural representante bajo el tipo int.

binPosToInt :: BinPos  -> Int
binPosToInt x = binAux x 0

-- | intToBinPos. Funcuión recursiva que dado un elemento que represente a un número natural de tipo Int devuelva su representante bajo el tipo BinarioPOs.

intToBinPos :: Int -> BinPos
intToBinPos 1 = U 
intToBinPos x = if mod x 2 == 0 then Cero ((intToBinPos (div x 2))) else Uno(intToBinPos (div x 2))

-- Ejemplos 

--Respuesta: Cero U
ejemplo1 = sucesor U 

--Respuesta: Uno (Cero (Uno (Uno U)))	
ejemplo2 = sucesor (Cero(Cero(Uno(Uno(U)))))

--Respuesta: Cero (Uno (Uno (Uno U)))
ejemplo3 = suma (Uno(Uno(Uno(U)))) (Uno(Uno(Uno(U))))

--Respuesta: Uno (Uno U)
ejemplo4 = suma (Cero(Cero(U))) (Uno(U))

--Respuesta: U
ejemplo5 = resta (Uno(Uno(Uno(U)))) (Uno(Uno(Uno(U))))

--Respuesta: Uno (Cero (Uno (Cero (Uno U))))
ejemplo6 = resta (Uno(Uno(Uno(Uno(Uno(U))))))  (Cero(Uno(Cero(U))))

--Respuesta: Cero (Cero (Cero (Uno (Cero (Cero (Cero (Cero U)))))))
ejemplo7 = producto (Uno(Cero(Cero(Uno U)))) (Cero(Uno(Cero U)))

--Respuesta: Cero (Cero (Uno (Cero U)))
ejemplo8 = producto (Cero(Uno(Cero U))) (Cero U)

--Respuesta: 31
ejemplo9 = binPosToInt (Uno(Uno(Uno(Uno U))))

--Respuesta: 42
ejemplo10 = binPosToInt (Cero(Uno(Cero(Uno(Cero U)))))

--Respuesta: Uno (Cero (Cero (Cero U)))
ejemplo11 = intToBinPos 17

--Respuesta: Uno (Uno (Cero (Cero (Cero U))))
ejemplo12 = intToBinPos 35

