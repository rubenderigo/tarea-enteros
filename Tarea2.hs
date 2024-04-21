{-#LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Tarea2 where

---------------------------------
--Nombre 1	: Ruben Derigo
--Nro. 1	: 287176
---------------------------------
--Nombre 2 	: Leandro Gugiato
--Nro. 2 	: 318621
---------------------------------

type N = Integer

--------------
--PROBLEMA 1--
--------------

suma_entre :: N -> N -> N
suma_entre m n
    | m > n = 0
	| otherwise = m + (suma_entre (m+1) n)

--1.
--suma_entre 2 5 = ...

--2.
{--
Es bien fundada porque...

El tamaño del problema que decrece es...
--}

--3.
suma_entre' :: N -> N -> N
suma_entre' m n = undefined

--4.
suma_entre_f :: (N -> N) -> N -> N -> N
suma_entre_f f m n = undefined

--5.
suma_i :: N -> N
suma_i n = undefined

--------------
--PROBLEMA 2--
--------------

--1.
es_divisor :: N -> N -> Bool
es_divisor n k = undefined

--2.
primer_divisor :: N -> N
primer_divisor n = undefined

--3.
es_primo :: N -> Bool
es_primo n = undefined

--------------
--PROBLEMA 3--
--------------

minimo_acotado :: (N -> Bool) -> N -> N -> N
minimo_acotado p m n
	| m > n = m
	| m <= n && p m = m
	| m <= n && not (p m) = minimo_acotado p (m+1) n

--1.
{--
Si ningun valor en el intervalo considerado cumple el predicado, entonces...
--}

--2.
primer_divisor' :: N -> N
primer_divisor' n = undefined

--3.
maximo_acotado :: (N -> Bool) -> N -> N -> N
maximo_acotado p m n = undefined

--4.
minimo_p :: (N -> Bool) -> N -> N
minimo_p p n
	| p n = n
	| not (p n) = minimo_p p (n+1)

{--
Esta función termina si...
--}

--------------
--PROBLEMA 4--
--------------

--1.
cantidad_p :: (N -> Bool) -> N -> N -> N
cantidad_p p m n = undefined

--2.
suma_p :: (N -> Bool) -> N -> N -> N
suma_p p m n = undefined

--3.
suma2_p :: (N -> Bool) -> N -> N -> N
suma2_p p m n = undefined

--4.
sumaf_p :: (N -> Bool) -> (N -> N) -> N -> N -> N
sumaf_p p f m n = undefined

--5.
todos_p :: (N -> Bool) -> N -> N -> Bool
todos_p p m n = undefined

--5.
existe_p :: (N -> Bool) -> N -> N -> Bool
existe_p p m n = undefined

-------
--FIN--
-------