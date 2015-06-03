module TypeInference (TypingJudgment, Result(..), inferType)

where

import Data.List(intersect)
import Exp
import Type
import Unification

------------
-- Errors --
------------
data Result a = OK a | Error String


--------------------
-- Type Inference --
--------------------
type TypingJudgment = (Env, AnnotExp, Type)


inferType :: PlainExp -> Result TypingJudgment
inferType e = case infer' e 0 of
    OK (_, tj) -> OK tj
    Error s -> Error s


infer' :: PlainExp -> Int -> Result (Int, TypingJudgment)

-- COMPLETAR DESDE AQUI

infer' (VarExp x) n = OK (n+1, (context, annotated, tipo))
  where 
    context = extendE emptyEnv x (TVar n)
    annotated = VarExp x
    tipo = TVar n

infer' ZeroExp n = OK (n, (emptyEnv, ZeroExp, TNat))

infer' (LamExp x a exp) n = case infer' exp n of
    Error s -> Error s
    OK (count, judgement) -> OK (fst count_type, (context, annotated, tipo_final))
      where
        context = removeE (fst_t judgement) x
        annotated = LamExp x tipo_x (snd_t judgement)
        tipo_final = TFun tipo_x (thd_t judgement)
        tipo_x = snd count_type
        count_type = extract count judgement x

extract :: Int -> TypingJudgment -> Symbol -> (Int, Type)
extract n (env, annotExp, tipo) exp
    | elem exp (domainE env) = (n, evalE env exp)
    | otherwise = (n+1, TVar n)



--infer' (AppExp x) n = 

fst_t (x, _, _) = x
snd_t (_, x, _) = x
thd_t (_, _, x) = x

--------------------------------
-- YAPA: Error de unificacion --
--------------------------------
uError :: Type -> Type -> Result (Int, a)
uError t1 t2 = Error $ "Cannot unify " ++ show t1 ++ " and " ++ show t2
