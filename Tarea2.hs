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
Es bien fundada porque... porque se trabaja y se hace recursion sobre intervalos, donde 
uno que se 'achica' (garantizando que la ejecucion va a terminar) y otro que aumenta.


El tamaño del problema que decrece es... que m va aumentando (aumentando el extemo inferior) y el extremo superior 
hacia n se va "achicando" hasta solaparse y cumplir el caso base.
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

-- utilizamos la funcion anomina '(\x -> x)' como parametro de suma_entre_f para obtener la identidad.

--------------
--PROBLEMA 2--
--------------

--1.
es_divisor :: N -> N -> Bool
es_divisor n k
	| k == 0 = error "no es posible dividir entre 0"
	| n == k = True
	| n < k = False
	| n > k = es_divisor (n-k) k


--2.
primer_divisor :: N -> N
primer_divisor n =
	let buscarDivisor k
			| k > n = n
			| es_divisor n k = k
			| otherwise = buscarDivisor (k + 1)
	in buscarDivisor 2


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
Si ningun valor en el intervalo considerado cumple el predicado, entonces... la funcion retorna m ya que se 'cae' 
en el primer caso base (solaparse).
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
cantidad_p p m n
	| m > n = 0
	| m <= n && p m = (cantidad_p p (m+1) n) + 1
	| m <= n && not (p m) = cantidad_p p (m+1) n

--2.
suma_p :: (N -> Bool) -> N -> N -> N
suma_p p m n
	| m > n = 0
	| m <= n && p m = (suma_p p (m+1) n) + m
	| m <= n && not (p m) = suma_p p (m+1) n

--3.
suma2_p :: (N -> Bool) -> N -> N -> N
suma2_p p m n
	| m > n = 0
	| m <= n && p m = (suma2_p p (m+1) n) + (m^2)
	| m <= n && not (p m) = suma2_p p (m+1) n

--4.
sumaf_p :: (N -> Bool) -> (N -> N) -> N -> N -> N
sumaf_p p f m n
	| m > n = 0
	| m <= n && p m = (sumaf_p p f (m+1) n) + f m
	| m <= n && not (p m) = sumaf_p p f (m+1) n

--5.
todos_p :: (N -> Bool) -> N -> N -> Bool
todos_p p m n
	| m > n = True
	| m <= n && not (p m) = False
	| m <= n && p m = todos_p p (m+1) n

--5.
existe_p :: (N -> Bool) -> N -> N -> Bool
existe_p p m n
	| m > n = False
	| m <= n && p m = True
	| m <= n && not (p m) = existe_p p (m+1) n

-------
--FIN--
-------