{-
- Estructuras Discretas 2017-1
- Recursion sobre EAB.
- Profesor: Dr. Favio Ezequiel Miranda Perea
- Ayudante: Victor Zamora Gutierrez
- Laboratorio: Fernando Abigail Galicia Mendoza
- Galeana Araujo Emiliano 
-}

module EAB where

-- | EAB. Tipo de datos que representa a las expresiones aritmetico booleanas (EAB)
data EAB = VNum Int | VBool Bool | Suma EAB EAB | Suc EAB | Pred EAB | Prod EAB EAB | Resta EAB EAB |
           Neg EAB | And EAB EAB

-- | IntBool. Union de los tipos Int y Bool
type IntBool = Either Int Bool

-- | Instancia de la clase show para las EAB.
instance Show EAB where
  show e = muestra e

-- | Instancia de la igualdad para las EAB.
instance Eq EAB where
  (==) e1 e2 = igual e1 e2
  e1 /= e2 = not ((==)e1 e2)

-- | Instancia del orden sobre las EAB.
instance Ord EAB where
  (<) e1 e2 = (precedencia e1) > (precedencia e2)
  (>=) e1 e2 = ((precedencia e1) > (precedencia e2)) || ((precedencia e1) == (precedencia e2))
  (<=) e1 e2 = ((precedencia e1) > (precedencia e2)) || ((precedencia e1) == (precedencia e2))
  (>) e1 e2 = (precedencia e1) > (precedencia e2)

-- | muestra. Funcion recursiva que dada una EAB la convierte a una cadena de texto.
muestra :: EAB -> String
muestra (VNum n)= show n
muestra (VBool b)= show b
muestra (Neg c)= "(~ "++muestra c++")"
muestra (Suc d)= show d++"++"
muestra (Pred g)= show g++"--"
muestra (Suma e1 e2) = "("++muestra e1 ++ "+" ++ muestra e2++")"
muestra (Resta e1 e2) = "("++muestra e1 ++ "-" ++ muestra e2++")"
muestra (Prod e1 e2) = "("++muestra e1 ++ "*" ++ muestra e2++")"
muestra (And e1 e2) = "("++muestra e1 ++ "^" ++ muestra e2++")"


-- | igual. Funcion recursiva que dada dos EAB determina si son iguales.
igual :: EAB -> EAB -> Bool
igual e1 e2 = case (e1,e2) of
              (VNum n,VNum m) -> ((==) n m)
              (VBool b1,VBool b2) -> ((==) b1 b2)
              (Suc e1, Suc e2) -> ((==)e1 e2)
              (Pred e1, Pred e2) -> ((==)e1 e2)
              (Neg e1, Neg e2) -> ((==)e1 e2)
              (Suma e11 e12, Suma e21 e22) -> (igual e11 e21) && (igual e12 e22)
              (Resta e11 e12, Resta e21 e22) -> (igual e11 e21) && (igual e12 e22)
              (Prod e11 e12, Prod e21 e22) -> (igual e11 e21) && (igual e12 e22)
              (And e11 e12, And e21 e22) -> (igual e11 e21) && (igual e12 e22)

-- | precedencia. Funcion recursiva que dada una EAB obtiene el valor entero
--asociado su nivel de precedencia.
precedencia :: EAB -> Int
precedencia e = case (e) of
        VNum x -> 0
        VBool x -> 0
        Suc e1 -> 1 +(precedencia e1)
        Pred e1 -> 1 + (precedencia e1)
        Neg e1 -> 2 + (precedencia e1)
        Suma e1 e2 -> 3 + (precedencia e1) + (precedencia e2)
        Resta e1 e2 -> 3 + (precedencia e1) + (precedencia e2)
        Prod e1 e2 -> 4 + (precedencia e1) + (precedencia e2)
        And e1 e2 -> 5 + (precedencia e1) + (precedencia e2)


-- | eval. Funcion recursiva que devuelve la evaluacion de una EAB.
eval :: EAB -> IntBool
eval (VNum n) = Left n
eval (VBool b) = Right b
eval (Suc e1) =
        let v1 = eval(e1) in
          case v1 of
          (Left n) -> Left(n+1)
          _ -> error "Error de ejecución"
eval (Pred e1) =
        let v1 = eval(e1) in
          case v1 of
          (Left n) -> Left(n-1)
          _ -> error "Error de ejecución"
eval (Neg e1) =
        let v1 = eval(e1) in
          case v1 of
          (Right n) -> Right(not(n))
          _ -> error "Error de ejecución"
eval (Suma e1 e2) =
        let v1 = eval e1 in
        let v2 = eval e2 in
          case (v1,v2) of
          (Left n,Left m) -> Left (n+m)
          _ -> error "Error de ejecución"
eval (Prod e1 e2) =
        let v1 = eval e1 in
        let v2 = eval e2 in
          case (v1,v2) of
          (Left n,Left m) -> Left (n*m)
          _ -> error "Error de ejecución"
eval (Resta e1 e2) =
        let v1 = eval e1 in
        let v2 = eval e2 in
          case (v1,v2) of
          (Left n,Left m) -> Left (n-m)
          _ -> error "Error de ejecución"
eval (And e1 e2) =
        let v1 = eval e1 in
        let v2 = eval e2 in
          case (v1,v2) of
          (Right n,Right m) -> Right (n&&m)
          _ -> error "Error de ejecución"




--Ejemplos

ej1 = muestra (Suma (VNum 1) (Suc (VNum 2)))
--Resultado: "(1+2++)"

ej2 = muestra (Neg (And (VBool True) (VBool False)))
--Resultado: "~(True^False)"

ej3 = muestra (Suma (VBool True) (Neg (VNum 1)))
--Resultado: "(True+~1)"

ej4 = igual (Suma (Suc (VNum 1)) (VNum 2)) (Suma (Suc (VNum 1)) (VNum 2))
--Resultado: True

ej5 = igual (Suma (Suc (VNum 1)) (VNum 2)) (Suma (Suc (VNum 1)) (VNum 3))
--Resultado: False

ej6 = igual (Neg (And (VBool True) (VBool False))) (Neg (And (VBool True) (VBool False)))
--Resultado: True

ej7 = igual (Neg (And (VBool True) (VBool True))) (Neg (And (VBool True) (VBool False)))
--Resultado: False

ej8 = precedencia (Suma (Suc (VNum 1)) (VNum 2))
--Resultado: 4

ej9 = precedencia (Neg (And (VBool True) (VBool False)))
--Resultado: 7

ej10 = precedencia (Suma (VBool True) (Neg (VNum 1)))
--Resultado: 5

ej11 = (Suma (VNum 1) (VNum 2)) < (Suc (VNum 1))
--Resultado: True

ej12 = (Neg (VBool True)) <= (Suc (VNum 1))
--Resultado: True

ej13 = (Suc (VNum 1)) < (Pred (VNum 2))
--Resultado: False

ej14 = (Suc (VNum 1)) > (Pred (VNum 2))
--Resultado: False

ej15 = (Suc (VNum 1)) >= (Pred (VNum 2))
--Resultado: True

ej16 = eval (Prod (Suma (VNum 1) (Suc (VNum 2))) (VNum 7))
--Resultado: Left 28

ej17 = eval (Neg (And (Neg (VBool True)) (VBool False)))
--Resultado: Right True

ej18 = eval (Neg (Suc (VBool True)))
--Resultado: *** Exception: Error de ejecucion.
