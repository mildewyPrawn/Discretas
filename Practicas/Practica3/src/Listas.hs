-- | Estructuras Discretas 2017-1
-- | Practica 1: Interpretaciones de la logica proposicional
-- | Profesor: Favio E. Miranda Perea
-- | Ayudante: Victor Zamora Gutierrez
-- | Laboratorio: Fernando A. Galicia Mendoza
-- | Integrantes:
-- | Galeana Araujo Emiliano	314032324


module Listas where

-- | Lista. Tipo que representa una lista de un tipo a.
data Lista a = Nil | Cons a (Lista a) deriving(Show)

-- | conS. Funcion que anexa un elemento al inicio de una lista.
conS :: a -> Lista a -> Lista a
conS a l = Cons a l

-- | cabeza. Funcion que devuelve la cabeza de una lista.
cabeza :: Lista a -> a
cabeza Nil = error "Lista vacía"
cabeza (Cons x l) = x

-- | cola. Funcion que devuelve la cola de una lista.
cola :: Lista a -> Lista a
cola Nil = error "Lista vacía"
cola (Cons x l) = l

-- | lon. Funcion recursiva que devuelve la longitud de una lista.
lon :: Lista a -> Int
lon Nil = 0
lon (Cons x lista) = 1 + lon lista

-- | conc. Funcion recursiva que devuelve la concatenacion de dos listas.
conc :: Lista a -> Lista a -> Lista a
conc (Cons x l) Nil = Cons x l
conc Nil (Cons x l) = Cons x l
conc (Cons a l1) (Cons x2 l2) = Cons a (conc l1 (Cons x2 l2))	

-- | esta. Funcion recursiva que indica si un elemento pertenece a una lista.
esta :: Eq a => a -> Lista a -> Bool
esta x Nil = False
esta x (Cons a l) = if x == a then True else (esta x l)

{-
Ejemplos
-}

ejemplo0 = conS 0 (Cons 1 (Cons 2 Nil))
--Resultado: Cons 0 (Cons 1 (Cons 2 Nil))

ejemplo1 = conS 'A' (Cons 'a' (Cons 'b' Nil))
--Resultado: Cons 'A' (Cons 'a' (Cons 'b' Nil))

ejemplo2 = conS 1 Nil
--Resulatdo: Cons 1 Nil

ejemplo3 = cabeza (Cons 1 (Cons 2 (Cons 3 Nil)))
--Resulatdo: 1

ejemplo4 = cabeza (Cons 'a' (Cons 'b' (Cons 'c' Nil)))
--Resulatdo: 'a'

ejemplo5 = cabeza Nil
--Resultado: ***Exception: Lista vacia.

ejemplo6 = cola (Cons 1 (Cons 2 (Cons 3 Nil)))
--Resulatdo: (Cons 2 (Cons 3 Nil))

ejemplo7 = cola (Cons 'a' (Cons 'b' (Cons 'c' Nil)))
--Resulatdo: (Cons 'b' (Cons 'c' Nil))

ejemplo8 = cola Nil
--Resultado: ***Exception: Lista vacia.

ejemplo9 = lon (Cons 1 (Cons 2 Nil))
--Resulatdo: 2

ejemplo10 = lon (Cons 'a' (Cons 'b' (Cons 'c' Nil)))
--Resultado: 3

ejemplo11 = lon Nil
--Resulatdo: 0

ejemplo12 = conc (Cons 1 (Cons 2 Nil)) (Cons 3 Nil)
--Resulatdo: Cons 1 (Cons 2 (Cons 3 Nil))}

ejemplo13 = conc (Cons 'a' (Cons 'b' (Cons 'c' Nil))) (Cons 'd' Nil)
--Resultado: Cons 'a' (Cons 'b' (Cons 'c' (Cons 'd' Nil)))}

ejemplo14 = conc Nil (Cons 1 Nil)
--Resulatdo: Cons 1 Nil

ejemplo15 = esta 2 (Cons 1 (Cons 2 Nil))
--Resultado: True

ejemplo16 = esta 'a' (Cons 'a' (Cons 'b' (Cons 'c' Nil)))
--Resultado: True

ejemplo17 =  esta 3 (Cons 1 Nil)
--Resultado: False
