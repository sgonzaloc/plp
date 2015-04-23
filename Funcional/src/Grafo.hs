module Grafo (Grafo, vacio, nodos, vecinos, agNodo, sacarNodo, agEje, lineal, union, clausura) where
import qualified Data.List as List

data Grafo a = G [a] (a -> [a])

instance (Show a) => Show (Grafo a) where
  show (G n e) = "[\n" ++ concat (map (\x -> " " ++ show x ++ " -> " ++ show (e x) ++ "\n") n) ++ "]"


-- ---------------------------------Sección 3--------- Grafos ---------------------------

-- Ejercicio 1
vacio :: Grafo a
vacio = G [] (\_ -> [])

-- Ejercicio 2
nodos :: Grafo a -> [a]
nodos (G ns _) = ns

-- Ejercicio 3
vecinos :: Grafo a -> a -> [a]
vecinos (G _ f) nodo = f nodo

-- Ejercicio 4
agNodo :: Eq a => a -> Grafo a -> Grafo a
agNodo n (G ns f) = G (add_if_not_present n ns) f

-- Ejercicio 5
sacarNodo :: Eq a => a -> Grafo a -> Grafo a
sacarNodo node (G ns f) = G  (rm_n ns) (\n -> if n == node then [] else rm_n (f n))
    where rm_n = List.delete node

-- Ejercicio 6
agEje :: Eq a => (a,a) -> Grafo a -> Grafo a
agEje (s, d) grafo@(G ns f) | valid_nodes = G ns new_f
                            | otherwise = grafo
    where
      valid_nodes = elem s ns && elem d ns
      new_f = (\x -> if x == s then add_if_not_present d (f x) else (f x))

-- Ejercicio 7
lineal :: Eq a => [a] -> Grafo a
lineal (n:[]) = G [n] (\_ -> [])
lineal ns = foldr add_edges vacio (zip ns $ tail ns)
    where add_edges = \(x,y) res -> agEje (x,y) (agNodo x (agNodo y res))

-- Ejercicio 8
union :: Eq a => Grafo a -> Grafo a -> Grafo a
union (G ns1 r1) (G ns2 r2) = G (List.nub (ns1 ++ ns2)) (unionRel r1 r2)
  where unionRel r1 r2 = \x -> List.nub ((r1 x) ++ (r2 x))

-- Ejercicio 9
clausura :: Eq a => Grafo a -> Grafo a
clausura (G ns fv) = G ns (\n -> (vecinosClausura fv) [n])

vecinosClausura :: Eq a => (a -> [a]) -> ([a] -> [a])
vecinosClausura fvs = puntofijo vecinosDeLista
  where vecinosDeLista = (\x -> List.nub (concat (x:(map fvs x))))

---- PRIVATE FUNCTIONS ----

-- Agrega un elemento a una lista si este no pertenece a la misma
add_if_not_present :: Eq a => a -> [a] -> [a]
add_if_not_present n ns | n `elem` ns = ns
                        | otherwise   = n:ns

-- Genera la funcion que devuelve el punto fijo para cualquier valor de entrada
puntofijo :: Eq a => (a -> a) -> (a -> a)
puntofijo f = (\x -> if x == (f x) then x else ((puntofijo f) (f x)))
