{-
- Estructuras discretas 2017-1
- Profesor: Favio E. Miranda Perea
- Ayudante: Victor Zamora Gutierrez
- Laboratorio: Fernando A. Galicia Mendoza
- Practica 1
- Integrantes:
-Galeana Araujo Emiliano 
-Márquez Castillo Irene 
-}

module Practica1 where

type Vertex = (Double,Double)

type Complejo = (Double,Double)

type Segundo = (Double,Double,Double)

--Funcion que obtiene la distancia entre dos puntos
dist :: Vertex -> Vertex -> Double
dist  (x1,y1) (x2,y2) = sqrt(((x2-x1)^2) + ((y2-y1)^2))

--Funcion que obtiene el área de un triángulo
areaTri :: Vertex -> Vertex -> Vertex -> Double
areaTri (x1,y1) (x2,y2) (x3,y3) = (((x1*y2)+(x2*y3)+(x3*y1))-((x1*y3)
          +(x3*y2)+(x2*y1)))/2

--Funcion que realiza la suma de dos complejos
suma :: Complejo -> Complejo -> Complejo
suma (z1, b1) (z2, b2) = (z1+z2, b1+b2)

--Funcion que realiza la resta de dos complejos
resta :: Complejo -> Complejo -> Complejo
resta (z1, b1) (z2, b2) = (z1-z2, b1-b2)

--Funcion que realiza la multiplicacion de dos complejos
prod :: Complejo -> Complejo -> Complejo
prod (z1, b1) (z2, b2) = ((z1*z2)+((b1*b2)*(-1)), (z1*b2)+(b1*z2))

--Funcion que realiza la division de dos complejos

division :: Complejo -> Complejo -> Complejo
division  (a, b) (c, d) = (((a*c)+(b*d))/((c*c)+(d*d)), ((b*c)-(a*d))/((c*c)+(d*d)))

--Funcion que obtiene el conjugado de un numero complejo
conjugado :: Complejo -> Complejo
conjugado  (a, b) = (a, (b*(-1)))

--Funcion que obtiene la parte real de un complejo
parteReal :: Complejo -> Double
parteReal (a,b) = a

--Funcion que obtiene la parte imaginaria de un complejo
parteImg :: Complejo -> Double
parteImg (a,b) = b

--Funcion que obtiene el modulo de un numero complejo
modulo :: Complejo -> Double
modulo  (a, b) = sqrt((a*a) + (b*b))

--Funcion que determina si un polinomio tiene raices reales
raizReal :: Segundo -> Bool
raizReal (a, b, c) = if (b*b-(4*a*c)) > 0 then True else False 

--Funcion que obtiene la raiz positiva de un polinomio
raizPos :: Segundo -> Complejo
raizPos (a,b,c) =  if raizReal (a,b,c) then ((-b/(2*a)) + (sqrt(b*b -(4*a*c)))/(2*a), 0) else (-b/(2*a), sqrt((b*b -(4*a*c))*(-1))/(2*a))

--Funcion que obtiene la raiz negativa de un polinomio
raizNeg :: Segundo -> Complejo
raizNeg (a,b,c) = if raizReal (a,b,c) then ((-b/(2*a)) - (sqrt(b*b -(4*a*c)))/(2*a), 0) else ((-b/(2*a)), sqrt((b*b -(4*a*c))*(-1))/(2*a))

--Ejemplos

dist1 = dist (3,4) (5,6)
--Resultado: 2.8284271247461903

dist2 = dist (0.45,12.34) (12,3.68)
--Resultado: 14.436000138542532


 {- NOTA: antes estaba escrito areaTri (-8,-2) (4,6) (-1,-5)
  -lo cambiamos agregando el areaTri1 = areaTri, de otra forma 
  -no se habría llamado a la función y no habría compilado.
  -}

areaTri1 = areaTri (-8,-2) (4,6) (-1,-5)
--  NOTA: haciendo los cálculos manuales, el resultado queda negativo
--Resultado: 46.00000000000002

areaTri2 =areaTri (-8,-2) (4,6) (1,5)
--Resultado: 6.000000000000054

suma1 = suma (1.2,4.24) (5.67,98.2)
--Resultado: (6.87,102.44)

suma2 = suma (-4,2.45) (2.4,-8.123)
--Resultado: (-1.6,-5.672999999999999)

resta1 = resta (1.2,4.24) (5.67,98.2)
--Resultado: (-4.47,-93.96000000000001)

resta2 = resta (-4,2.45) (2.4,-8.123)
--Resultado: (-6.4,10.573)

prod1 = prod (-24.56,12.67) (-123,6.54)
--Resultado: (2938.0181999999995,-1719.0324)

prod2 = prod (3.78,34.6) (2.46,62.5)
--Resultado: (-2153.2012,321.366)

div1 = division (-123,6.54) (-24.56,12.67)
--Resultado: (4.063965380095519,1.8302296973049768)

div2 = division (-123,6.54) (0,0)
--Resultado: (NaN,NaN) debido que estamos dividiendo entre cero

conjugado1 = conjugado (-123,6.54)
--Resultado: (-123,-6.54)

conjugado2 = conjugado (2.4,-8.123)
--Resultado: (2.4,8.123)

parteR1 = parteReal (2.4,-8.123)
--Resultado: 2.4

parteR2 = parteReal (-8.123,34.77)
--Resultado: -8.123

parteI1 = parteImg (2.4,-8.123)
--Resultado: -8.123

parteI2 = parteImg (-8.123,34.77)
--Resultado: 34.77

mod1 = modulo (-123,6.54)
--Resultado: 123.17374557916148

mod2 = modulo (0,0)
--Resultado: 0.0

raizR1 = raizReal (1,0,-1)
--Polinomio (x^2 - 1), resultado: True

raizR2 = raizReal (1,0,1)
--Polinomio (x^2 + 1), resultado: False

raizPos1 = raizPos (1,0,-1)
--Resultado: (1.0,0.0)

raizPos2 = raizPos (1,0,1)
--Resultado: (-0.0,1.0)

raizNeg1 = raizNeg (1,0,-1)
--Resultado: (-1.0,0.0)

raizNeg2 = raizNeg (1,0,1)
--Resultado: (-1.0,1.0)


