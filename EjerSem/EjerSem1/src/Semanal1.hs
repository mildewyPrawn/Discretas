-- | Estructuras Discretas 2017-1
-- | Semanal 1: Funciones sobre listas
-- | Profesor: Favio E. Miranda Perea
-- | Ayudante: Victor Zamora Gutierrez
-- | Laboratorio: Fernando A. Galicia Mendoza
-- | Integrantes: Emiliano Galeana Araujo	314032324	galeanaara@ciencias.unam.mx

module Semanal1 where

{-
------------------------------------------------------------------------------------------
-Parte 1
------------------------------------------------------------------------------------------
-}

-- | sumaGauss. Funcion que representa la formula de Gauss.
--
-- --> sumaGauss 5 = 15
-- --> sumaGauss 901 = 406351
sumaGauss :: Int -> Int
sumaGauss x = (x*(x+1)) `div` 2 

-- | serieGauss. funcion que obtiene la lista que representa
-- | primeros n elementos de la serie de Gauss.
--
-- --> serieGauss 10 = [0,1,3,6,10,15,21,28,36,45,55]
-- --> serieGauss 20 = [0,1,3,6,10,15,21,28,36,45,55,66,78,91,105,120,136,153,171,190,210]
serieGauss :: Int -> [Int]
serieGauss x = [ sumaGauss x | x <- [0..x]]

{-
------------------------------------------------------------------------------------------
-Parte 2
------------------------------------------------------------------------------------------
-}

-- | carAAscii. Funcion que obtiene el decimal representante de un caracter.
--
-- --> carAAscii 'f'
-- --> carAAscii 'h'
carAAscii :: Char -> Int
carAAscii x = if x == ' ' then 0 else fromEnum x

-- | cifradoAscii. Funcion que obtiene la cadena de decimales representantes de un cadena
-- | texto.
--
-- --> cifradoAscii "haskell"
-- --> cifradoAscii "esto es un mensaje oculto"
cifradoAscii :: String -> [Int]
cifradoAscii x = map carAAscii x

--Ejemplos

sumaGauss1 = sumaGauss 112
--Resultado: 6328

sumaGauss2 = sumaGauss 1822
--Resultado: 1660753

sumaGauss3 = sumaGauss 9823
--Resultado: 48250576

serieGauss1 = serieGauss 22
--Resultado: [0,1,3,6,10,15,21,28,36,45,55,66,78,91,105,120,136,153,171,190,210,231,253]

serieGauss2 = serieGauss 14
--Resultado: [0,1,3,6,10,15,21,28,36,45,55,66,78,91,105]

serieGauss3 = serieGauss 18
--Resultado: [0,1,3,6,10,15,21,28,36,45,55,66,78,91,105,120,136,153,171]

carAAscii1 = carAAscii 'u'
--Resultado: 117

carAAscii2 = carAAscii 'w'
--Resultado: 119

carAAscii3 = carAAscii 'q'
--Resultado: 113

cifradoAscii1 = cifradoAscii "termine la practica"
--Resultado: [116,101,114,109,105,110,101,0,108,97,0,112,114,97,99,116,105,99,97]

cifradoAscii2 = cifradoAscii "este mensaje es privado"
--Resultado: [101,115,116,101,0,109,101,110,115,97,106,101,0,101,115,0,112,114,105,118,97,100,111]

cifradoAscii3 = cifradoAscii "no hay algo mejor que programar"
--Resultado: [110,111,0,104,97,121,0,97,108,103,111,0,109,101,106,111,114,0,113,117,101,0,112,114,111,103,114,97,109,97,114]
