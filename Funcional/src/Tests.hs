import Grafo
import Tipos
import Lomoba
import Parser
import Test.HUnit

-- evaluar t para correr todos los tests
t = runTestTT allTests

allTests = test [
  "parser" ~: testsParser,
  "grafo" ~: testsGrafo,
  "lomoba" ~: testsLomoba
  ]

testsParser = test [
  (Var "p")                       ~=? (parse "p"),
  (And (Var "p") (Var "q"))       ~=? (parse "p && q"),
  (Or (Var "p") (Var "q"))        ~=? (parse "p || q"),
  (Or (Not (Var "p")) (Var "q"))  ~=? (parse "!p || q"),
  (And (D (Var "p")) (Var "q"))   ~=? (parse "<>p && q"),
  (And (B (Var "p")) (Var "q"))   ~=? (parse "[]p && q"),
  (D (And (Var "p") (Var "q")))   ~=? (parse "<>(p && q)"),
  (B (And (Var "p") (Var "q")))   ~=? (parse "[](p && q)")
  ]


-- Variables auxiliares para los tests --

g_lineal = lineal [1..4]
g_clausura_lineal = clausura g_lineal
g_clausura_union = clausura (union g_lineal (lineal [1,5,6,7]))

-----------------------------------------

testsGrafo = test [
  -- Test Nodos --
  -- [] ~~? (nodos vacio), -- No se puede testear por un problema de tipos
  [1] ~~? (nodos (agNodo 1 vacio)),
  [1,2] ~~? (nodos (agNodo 2 (agNodo 1 vacio))),

  -- Test Vecinos --
  []  ~~? (vecinos (agNodo 1 vacio) 1),
  []  ~~? (vecinos (agEje (2,1) (agNodo 1 vacio)) 1),
  []  ~~? (vecinos (agEje (1,1) (agNodo 1 vacio)) 2),
  [1] ~~? (vecinos (agEje (1,1) (agNodo 1 vacio)) 1),

  -- Test sacarNodo --
  []     ~~? (nodos (sacarNodo 1 vacio)),
  []     ~~? (nodos (sacarNodo 1 (agNodo 1 vacio))),
  [1..3] ~~? (nodos (sacarNodo 4 g_lineal)),
  []     ~~? (vecinos (sacarNodo 4 g_lineal) 3),

  -- Test lineal --
  [9]    ~~? (nodos   (lineal [9])),
  [1..4] ~~? (nodos   g_lineal),
  [2]    ~~? (vecinos g_lineal 1),
  []     ~~? (vecinos g_lineal 4),

  -- Test union --
  [1]     ~~? (nodos   (union vacio (lineal [1]))),
  [1,2]   ~~? (nodos   (union (lineal [2]) (lineal [1]))),
  [1..10] ~~? (nodos   (union (lineal [5..10]) (lineal [1..5]))),
  []      ~~? (vecinos (union vacio (lineal [1])) 1),
  []      ~~? (vecinos (union (lineal [2]) (lineal [1])) 1),
  [6]     ~~? (vecinos (union (lineal [5..10]) (lineal [1..5])) 5),

  -- Test Clausura --
  [1..4]  ~~? (nodos   g_clausura_lineal),
  [1..4]  ~~? (vecinos g_clausura_lineal 1),
  [2,3,4] ~~? (vecinos g_clausura_lineal 2),
  [3,4]   ~~? (vecinos g_clausura_lineal 3),
  [4]     ~~? (vecinos g_clausura_lineal 4),


  [1..7]  ~~? (nodos   g_clausura_union),
  [1..7]  ~~? (vecinos g_clausura_union 1),
  [2,3,4] ~~? (vecinos g_clausura_union 2),
  [3,4]   ~~? (vecinos g_clausura_union 3),
  [4]     ~~? (vecinos g_clausura_union 4),
  [5,6,7] ~~? (vecinos g_clausura_union 5),
  [6,7]   ~~? (vecinos g_clausura_union 6),
  [7]     ~~? (vecinos g_clausura_union 7)

  ]

-- Variables auxiliares para los tests --

m_lineal = K g_lineal (\_ -> [1,3])
m_clausura_lineal = K g_clausura_lineal (\_ -> [1,3])
exp_p = parse "p"

----------------------------------------


testsLomoba = test [
  -- Test visibilidad --
  0 ~=? visibilidad exp_p,
  1 ~=? visibilidad (parse "<>p"),
  2 ~=? visibilidad (parse "<>!<>p"),
  2 ~=? visibilidad (parse "<><>p || <><>q"),
  3 ~=? visibilidad (parse "<>(<>p || <><>q)"),
  3 ~=? visibilidad (parse "[](<>p || <>[]q)"),

  -- Test extraer --
  ["p"]      ~~? extraer exp_p,
  ["p"]      ~~? extraer (parse "p || !p"),
  ["p", "q"] ~~? extraer (parse "p && q"),

  -- Test eval --
  False ~=? eval (K vacio (\_ -> [])) 1 exp_p,
  True  ~=? eval (K (lineal [1]) (\_ -> [1])) 1 exp_p,
  False ~=? eval m_lineal 1 (parse "p && !q"),
  False ~=? eval m_lineal 2 (parse "p && q"),
  True  ~=? eval m_clausura_lineal 1 (parse "<>p"),
  False ~=? eval m_clausura_lineal 2 (parse "[]p"),

  -- Test valeEn --
  []  ~~? (valeEn exp_p (K vacio (\_ -> []))),
  [1] ~~? (valeEn (parse "p && q") (K (lineal [1]) (\_ -> [1]))),

  -- Test quitar --
  False  ~=? cierto m_clausura_lineal exp_p,
  True   ~=? cierto (quitar exp_p m_clausura_lineal) exp_p,

  -- Test cierto --
  True  ~=? cierto (K g_clausura_lineal (\_ -> []) )     (parse "!p"),
  True  ~=? cierto (K g_clausura_lineal (\_ -> [1..4]) ) exp_p,
  False ~=? cierto m_clausura_lineal (parse "<>p"),
  False ~=? cierto m_clausura_lineal (parse "[]p")

  ]

---------------
--  helpers  --
---------------

-- idem ~=? pero sin importar el orden
(~~?) :: (Ord a, Eq a, Show a) => [a] -> [a] -> Test
expected ~~? actual = (sort expected) ~=? (sort actual)
  where
    sort = foldl (\r e -> push r e) []
    push r e = (filter (e<=) r) ++ [e] ++ (filter (e>) r)
