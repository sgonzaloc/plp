module Grafo (Grafo, vacio, nodos, vecinos, agNodo, sacarNodo, agEje, lineal, union, clausura) where

data Grafo a = G [a] (a -> [a])

instance (Show a) => Show (Grafo a) where
  show (G n e) = "[\n" ++ concat (map (\x -> " " ++ show x ++ " -> " ++ show (e x) ++ "\n") n) ++ "]"


-- ---------------------------------Sección 3--------- Grafos ---------------------------

-- Ejercicio 1
vacio :: Grafo a
vacio = G [] (\x -> [])

-- Ejercicio 2
nodos :: Grafo a -> [a]
nodos (G ns r) = ns 

-- Ejercicio 3
vecinos :: Grafo a -> a -> [a]
vecinos (G ns r) n = r n

-- Ejercicio 4
agNodo :: a -> Grafo a -> Grafo a
agNodo n (G ns r) = G (n:ns) r

-- Ejercicio 5
sacarNodo :: Eq a => a -> Grafo a -> Grafo a
sacarNodo n (G ns r) = 
	G (sacar n ns) (relacionesSin r n)
	where
		relacionesSin r n = \x -> if (x == n) then [] else (sacar n (r x))

--PRE: n, m in nodos G
-- Ejercicio 6
agEje :: Eq a => (a,a) -> Grafo a -> Grafo a
agEje (n, m) (G ns r) = G ns (relacionesCon r (n, m))
	where relacionesCon r (n, m) = \x -> if (x == n) 
		then m:(r x) 
		else (r x)

-- Ejercicio 7
lineal :: Eq a => [a] -> Grafo a
lineal ns 		= 
	G ns (relLin ns)
	where
		len = length ns
		relLin ns = \x -> if ((len == 0) || (last ns == x))
			then [] 
			else [ns!!(y+1)| y <- [0.. len-1], ns!!y == x]
		

-- Ejercicio 8
union :: Eq a => Grafo a -> Grafo a -> Grafo a
union (G ns1 r1) (G ns2 r2) = G (sinRepetidos (ns1++ns2)) (unionRel r1 r2)
	where unionRel vecinos1 vecinos2 = \x -> sinRepetidos ((vecinos1 x) ++ (vecinos2 x))

-- Ejercicio 9
clausura :: Grafo a -> Grafo a
clausura = undefined

sacar :: Eq a => a -> [a] -> [a]
sacar n xs = [x | x <- xs, not (x == n)]

sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos xs = [xs!!i | i <- [0..(length xs)-1], not (elem (xs!!i) (drop (i+1) xs))]




