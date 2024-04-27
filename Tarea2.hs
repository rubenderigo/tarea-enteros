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
{--
suma_entre 2 5 = 14


2 + (suma entre (2+1) 5)
2 + (3 + (suma entre (3+1) 5) ) =
2 + (3 + (4 + (suma entre (4+1) 5))  =
2 + (3 + (4 + (5 + (suma entre (5+1) 5))
2 + (3 + (4 + (5 + (0))) = 14
--}


--2.
{--
Es bien fundada porque... porque se trabaja y se hace recursion sobre intervalos (existe un principio y un fin). 

El tamaño del problema que decrece es... que m va aumentando (aumentando el extemo inferior) y el extremo superior hacia n
se va "achicando" hasta solaparse y cumplir el caso base.
--}


--3.
suma_entre' :: N -> N -> N
suma_entre' m n
	| m > n = 0
	| otherwise = (suma_entre' m (n-1)) + n


--4.
suma_entre_f :: (N -> N) -> N -> N -> N
suma_entre_f f m n
	| m > n = 0
	| otherwise = suma_entre_f f m (n-1) + f (n)


-- para testear suma_entre_f
doble :: N -> N
doble 0 = 0
doble k = (doble (k-1)) + 2


--5.
suma_i :: N -> N
suma_i n = suma_entre_f (\x -> x) n 0


--------------
--PROBLEMA 2--
--------------

--1.
es_divisor :: N -> N -> Bool
es_divisor n k
	| n == k = True
	| k == 0 = False
	| n < k = False
	| n > k = es_divisor (n-k) k


--2.
primer_divisor :: N -> N
primer_divisor n 
	| es_divisor n 2 = 2
	| es_divisor n 3 = 3
	| es_divisor n n = n


--3.
es_primo :: N -> Bool
es_primo n
	| primer_divisor n == n = es_divisor n 1
	| otherwise = False

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
Si ningun valor en el intervalo considerado cumple el predicado, entonces... la funcion retorna m ya que se 'cae' en el
primer caso base (solaparse).
--}

--2.
primer_divisor' :: N -> N
primer_divisor' n = minimo_acotado (es_divisor n) 2 n

--3.
maximo_acotado :: (N -> Bool) -> N -> N -> N
maximo_acotado p m n
	| m > n = n
	| m <= n && p n = n
	| m <= n && not (p n) = maximo_acotado p m (n-1)

-- para testear maximo_acotado
par :: N -> Bool
par 0 = True
par n = not (par (n-1))

--4.
minimo_p :: (N -> Bool) -> N -> N
minimo_p p n
	| p n = n
	| not (p n) = minimo_p p (n+1)

{--
Esta función termina si... se cumple el predicado con el valor de n que se encuentre.
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