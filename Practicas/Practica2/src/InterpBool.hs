-- | Estructuras Discretas 2017-1
-- | Practica 1: Interpretaciones de la logica proposicional
-- | Profesor: Favio E. Miranda Perea
-- | Ayudante: Victor Zamora Gutierrez
-- | Laboratorio: Fernando A. Galicia Mendoza
-- | Integrantes: Emiliano Galeana Araujo	314032324	galeanaara@ciencias.unam.mx

module InterpBool where

-- | Prop. Tipo que representa una formula de la logica proposicional.
data Prop = Var String | Neg Prop | Conj Prop Prop | Disy Prop Prop | Impl Prop Prop | Syss Prop Prop deriving(Show)

-- | Modelo. Tipo que representa un modelo, es decir, es una lista de variables proposicionales
-- | que se consideran verdaderas.
type Modelo = [String]

-- | neg. Funcion que devuelve la negacion de una constante booleana.
--
-- --> neg False = True
neg :: Bool -> Bool
neg  True = False
neg  False = True

-- | conj. Funcion que devuelve la conjuncion de dos constantes booleanas.
--
-- --> conj True True = True
conj :: Bool -> Bool -> Bool
conj True True = True 
conj False False = False
conj True False = False
conj False True = False

-- | disy. Funcion que devuelve la disyuncion de dos constantes booleanas.
--
-- --> disy True False = True
disy :: Bool -> Bool -> Bool
disy True True = True
disy True False = True
disy False True = True
disy False False = False

-- | impl. Funcion que devuelve la implicacion de dos constantes booleanas.
--
{-- La tabla de verdad de la implicación sería:
 --     1	1	1
 -- 	1	0	0
 --	0	1	1
 -- 	0	0	1
 --}
-- --> impl False True = False
impl :: Bool -> Bool -> Bool
impl True True = True
impl True False = False
impl False True = True
impl False False = True

-- | syss. Funcion que devuelve la doble condicional de dos constantes booleanas.
--
-- --> syss True False = False
syss :: Bool -> Bool -> Bool
syss True True = True
syss True False = False
syss False True = False
syss False False = True

-- | xor. Funcion que devuelve la disyuncion exclusiva de dos constantes booleanas.
--
-- --> xor True False = True
xor :: Bool -> Bool -> Bool
xor True True = False
xor True False = True
xor False True = True
xor False False = False

-- | nand. Funcion que devuelve la conjuncion negada de dos constantes booleanas.
--
-- --> nand True False = True
nand :: Bool -> Bool -> Bool
nand True True = False
nand True False = True 
nand False True = True 
nand False False = True

-- | nor. Funcion que devuelve la disyuncion negada de dos constantes booleanas.
--
-- --> nor True False = True
{-- La tabla de verdad de la disyunción sería:
 --     1	1	1
 -- 	1	0	1
 --	0	1	1
 -- 	0	0	0
 -- si niego los resultados, me daría:
 --	0
 -- 	0
 -- 	0
 --	1
 --}
nor :: Bool -> Bool -> Bool
nor True True = False
nor True False = False
nor False True = False 
nor False False =True

-- | i. Funcion que implementa la funcion de interpretacion sobre un modelo en
-- una formula de la logica proposicional.
--
-- --> i ["p","q","r"] (Conj (Var "p") (Var "q")) = True
-- --> i ["p","q","r"] (Conj (Var "p") (Neg (Var "q"))) = False
-- --> i ["p","q","r"] (Impl (Var "s") (Var "q")) = True
-- --> i ["p","q","r"] (Syss (Var "s") (Neg (Var "q"))) = True
-- --> i ["p","q","r"] (Disy (Var "s") (Var "t")) = False
i :: Modelo -> Prop -> Bool
i ["p","q","r"](Var p)= elem p ["p","q","r"]
i ["p","q","r"](Neg f)= neg (i ["p","q","r"] f)
i ["p","q","r"](Conj f1 f2)= conj (i ["p","q","r"] f1)(i ["p","q","r"] f2)
i ["p","q","r"](Disy f1 f2)= disy (i ["p","q","r"] f1)(i ["p","q","r"] f2)
i ["p","q","r"](Impl f1 f2)= impl (i ["p","q","r"] f1)(i ["p","q","r"] f2)
i ["p","q","r"](Syss f1 f2)= syss (i ["p","q","r"] f1)(i ["p","q","r"] f2)

{--
 --	B: Es un bebé.
 --	M: Puede manejar un cocodrilo.
 --	L: Es lógico.
 -- 	D: Es despistado.
 --
 --}

-- | e1. Funcion que representa el enunciado: Todos los bebes son ilogicos.
e1 :: Prop
e1 = Impl (Var"b") (Neg (Var"l"))


-- | e2. Funcion que representa el enunciado: Nadie que sea despistado puede manejar un cocodrilo.
e2 :: Prop
e2 = Impl (Neg (Var"d")) (Var"m")

-- | e3. Funcion que representa el enunciado: Las personas ilogicas son despistadas.
e3 :: Prop
e3 = Impl (Neg (Var "l")) (Var "d")

-- | e. Funcion que representa el enunciado: e1 /\ e2 /\ e3 -> (B -> ¬M)
e :: Prop
e = Impl (Conj e1 (Conj e2 e3)) ( Impl (Var "B") ((Neg (Var "M"))))
