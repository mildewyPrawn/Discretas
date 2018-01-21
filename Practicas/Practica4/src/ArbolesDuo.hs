{-
- Estructuras Discretas 2017-1
- Recursion sobre EAB.
- Profesor: Dr. Favio Ezequiel Miranda Perea
- Ayudante: Victor Zamora Gutierrez
- Laboratorio: Fernando Abigail Galicia Mendoza
- Emiliano Galeana Araujo 314032324 galeanaara@ciencias.unam.mx
-}

module ArbolesDuo where 

data Binario a = Hoja a | Nodo a (Binario a) (Binario a) deriving Show

data ABinDuo a b = HojaA a | HojaB b | NodoA a (ABinDuo a b) (ABinDuo a b)| NodoB b (ABinDuo a b) (ABinDuo a b) deriving Show

-- | ABin. Tipo que implementa un arbol binario no vacio de dos tipos,
-- utilizando el tipo de datos Binario.
type ABin a b = Binario (Either a b)


-- | nodosA. Funcion que dado un arbol binario de dos tipos, devuelve el
-- numero de nodos del primer tipo.
nodosA :: ABin a b -> Int
nodosA a = case (a) of
	Hoja a -> case a of 
		Left a -> 1
		Right a ->0
--nodosA l m n = case (l m n) of 
	Nodo l m n -> case l of
		Left l -> 1 + nodosA m + nodosA n
		Right l -> 0 + nodosA m + nodosA n
-- Ayuda clase ¿?
	{--Nodo l m n -> case l m n of 

--}

-- | nodosB. Funcion que dado un arbol binario de dos tipos, devuelve el
-- numero de nodos del segundo tipo.
nodosB :: ABin a b -> Int
nodosB b = case (b) of
	Hoja b -> case b of
		Left b -> 0
		Right b -> 1
 
	Nodo x y z -> case x of
		Left x -> 0 + nodosB y + nodosB z
		Right x -> 1 + nodosB y + nodosB z

-- | preordenA. Funcion que dado un arbol binario de dos tipos, devuelve
-- la lista resultante del recorrido preorden sobre el primer tipo.
preordenA :: ABin a b -> [a]
preordenA a = case (a) of
	Hoja a -> case a of
		Left a -> [a]
		Right a -> []
	Nodo l m n -> case l of
		Left l -> [l]++preordenA m++preordenA n
		Right l -> preordenA m++preordenA n
	

-- | preordenB. Funcion que dado un arbol binario de dos tipos, devuelve
-- la lista resultante del recorrido preorden sobre el segundo tipo.
preordenB :: ABin a b -> [b]
preordenB b = case (b) of
	Hoja b -> case b of
		Left b -> []
		Right b -> [b]
	Nodo x y z -> case x of
		Left x -> preordenB y++preordenB z
		Right x -> [x]++preordenB y ++ preordenB z
	

-- | inordenA. Funcion que dado un arbol binario de dos tipos, devuelve
-- la lista resultante del recorrido inorden sobre el primer tipo.
inordenA :: ABin a b -> [a]
inordenA a = case (a) of
	Hoja a -> case a of
		Left a -> [a]
		Right a -> []
	Nodo l m n -> case l of
		Left l -> inordenA m++ [l]++inordenA n
		Right l -> inordenA m++ inordenA n

-- | inordenB. Funcion que dado un arbol binario de dos tipos, devuelve
-- la lista resultante del recorrido inorden sobre el segundo tipo.
inordenB :: ABin a b -> [b]
inordenB b = case (b) of
	Hoja b -> case b of
		Left b -> []
		Right b -> [b]
	Nodo x y z -> case x of
		Left x -> preordenB y++inordenB z
		Right x -> inordenB y ++[x]++ inordenB z
-- | postordenA. Funcion que dado un arbol binario de dos tipos, devuelve
-- la lista resultante del recorrido postorden sobre el primer tipo.
postordenA :: ABin a b-> [a]
postordenA a = case (a) of
	Hoja a -> case a of
		Left a -> [a]
		Right a -> []
	Nodo l m n -> case l of
		Left l -> postordenA m++preordenA n++[l]
		Right l -> postordenA m++ preordenA n

-- | postordenB. Funcion que dado un arbol binario de dos tipos, devuelve
-- la lista resultante del recorrido postorden sobre el segundo tipo.
postordenB :: ABin a b -> [b]
postordenB b = case (b) of
	Hoja b -> case b of
		Left b -> []
		Right b -> [b]
	Nodo x y z -> case x of
		Left x -> postordenB y++ preordenB z
		Right x -> postordenB y ++ preordenB z++[x]




--Define las siguientes funciones recursivas para el tipo de datos ABinDuo




-- | nodosA2. Funcion que dado un arbol binario de dos tipos, devuelve el
-- numero de nodos del primer tipo.
nodosA2 :: ABinDuo a b -> Int
nodosA2 a = case (a) of
	HojaA a -> 1
	HojaB a -> 0
 
	NodoA l m n -> 1 + nodosA2 m + nodosA2 n
	NodoB l m n -> 0 + nodosA2 m + nodosA2 n

-- | nodosB2. Funcion que dado un arbol binario de dos tipos, devuelve el
-- numero de nodos del segundo tipo.
nodosB2 :: ABinDuo a b -> Int
nodosB2 b = case (b) of
	HojaA b -> 0
	HojaB b -> 1
 
	NodoA x y z -> 0 + nodosB2 y + nodosB2 z
	NodoB x y z -> 1 + nodosB2 y + nodosB2 z

-- | preordenA2. Funcion que dado un arbol binario de dos tipos, devuelve
-- la lista resultante del recorrido preorden sobre el primer tipo.
preordenA2 :: ABinDuo a b -> [a]
preordenA2 a = case (a) of
	HojaA a -> [a]
	HojaB a -> []
 
	NodoA l m n -> [l]++ preordenA2 m++preordenA2 n
	NodoB l m n -> preordenA2 m ++ preordenA2 n
-- | preordenB2. Funcion que dado un arbol binario de dos tipos, devuelve
-- la lista resultante del recorrido preorden sobre el segundo tipo.
preordenB2 :: ABinDuo a b -> [b]
preordenB2 b = case (b) of
	HojaA b -> []
	HojaB b -> [b]

	NodoA x y z -> preordenB2 y++ preordenB2 z
	NodoB x y z -> [x]++ preordenB2 y++ preordenB2 z

-- | inordenA2. Funcion que dado un arbol binario de dos tipos, devuelve
-- la lista resultante del recorrido inorden sobre el primer tipo.
inordenA2 :: ABinDuo a b -> [a]
inordenA2 a = case (a) of
	HojaA a -> [a]
	HojaB a -> []

	NodoA l m n-> inordenA2 m++ [l]++ inordenA2 n
	NodoB l m n -> inordenA2 m++ inordenA2 n

-- | inordenB2. Funcion que dado un arbol binario de dos tipos, devuelve
-- la lista resultante del recorrido inorden sobre el segundo tipo.
inordenB2 :: ABinDuo a b -> [b]
inordenB2 b = case (b) of 
	HojaA b -> []
	HojaB b -> [b]

	NodoA x y z -> inordenB2 y ++ inordenB2 z
	NodoB x y z -> inordenB2 y ++ [x] ++ inordenB2 z

-- | postordenA2. Funcion que dado un arbol binario de dos tipos, devuelve
-- la lista resultante del recorrido postorden sobre el primer tipo.
postordenA2 :: ABinDuo a b -> [a]
postordenA2 a = case (a) of
	HojaA a -> [a]
	HojaB a -> []

	NodoA l m n -> inordenA2 m ++ inordenA2 n ++ [l] 
	NodoB l m n -> inordenA2 m ++ inordenA2 n 

-- | postordenB2. Funcion que dado un arbol binario de dos tipos, devuelve
-- la lista resultante del recorrido postorden sobre el segundo tipo.
postordenB2 :: ABinDuo a b -> [b]
postordenB2 b = case (b) of 
	HojaA b -> []
	HojaB b -> [b]

	NodoA x y z -> inordenB2 y ++ inordenB2 z
	NodoB x y z -> inordenB2 y ++ inordenB2 z ++ [x]



--4Define una función que transforme un elemento de tipo ABin a el tipo ABinDuo.
-- Firma de la función:

--binToBin :: BinarioDuo a b → BinDuo a b
--No lo logré hacer, lo siento :C 



--5Da seis expresiones tales que:
--Tres representen árboles binarios que sean de tipo BinarioDuo, los tres árboles deben ser distintos.
arbolito = Nodo(Left 1)(Hoja(Right 'E')) (Nodo(Left 2)(Hoja(Right 'm'))(Nodo(Right 'i')(Hoja(Right 'e'))(Nodo(Left 3)(Hoja(Right 's'))(Nodo(Right 'c')(Hoja(Right 'h'))(Nodo(Left 4)(Hoja(Right 'i'))(Nodo(Right 'd')(Hoja(Right 'o')) (Hoja (Left 5))))))))
arbol = Nodo(Left 1)(Hoja(Right 'E')) (Nodo(Left 2)(Hoja(Right 'm'))(Nodo(Right 'i')(Hoja(Right 'l'))(Nodo(Left 3)(Hoja(Right 'i'))(Nodo(Right 'a')(Hoja(Right 'n'))(Nodo(Left 4)(Hoja(Right 'o'))(Nodo(Left 5)(Hoja(Left 6)) (Hoja (Left 7))))))))
arbolote = Nodo(Left 1)(Hoja(Right 'h')) (Nodo(Left 2)(Hoja(Right 'o'))(Nodo(Right 'l')(Hoja(Right 'a'))(Nodo(Left 3)(Hoja(Right ' '))(Nodo(Right 'm')(Hoja(Right 'u'))(Nodo(Left 4)(Hoja(Right 'n'))(Nodo(Right 'd')(Hoja(Left 6)) (Hoja (Right 'o'))))))))

--Tres representen árboles binarios que sean de tipo BinDuo, los tres árboles deben ser distintos.
arbolitoA2 = NodoA 09 (HojaB 'H')(NodoA 05(HojaB 'o')(NodoB 'L'(HojaB 'a')(HojaA 1998))) 
arbolA2 = NodoA 13 (HojaB 'P')(NodoA 08(HojaB 'a')(NodoB 'c'(HojaB 'o')(HojaA 1999)))
arboloteA2 = NodoA 10 (HojaB 'V')(NodoA 05(HojaB 'a')(NodoB 'l'(HojaB 'e')(HojaA 2002)))

