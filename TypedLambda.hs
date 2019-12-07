{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}


module TypedLambda where

import Data.Map (Map, lookup, empty, insert)
import Data.Maybe (fromJust)
import Language.Grammars.AspectAG
import Language.Grammars.AspectAG.TH
import Prelude (String, ($), (<*>), (<$>), Eq, Read, Show, show, (++), error, (==))

-- Modelar cálculo lambda simplemente tipado.
-- Lenguaje de términos:
-- t :: = x           (variables)
--     | \x^ty . t    (lambda abstracciones)
--     | t t          (aplicaciones)
-- Lenguaje de tipos:
-- ty ::= b | ty -> ty
-- b ::= bool | int | char | float

type Variable = String
data Tys = B | C | I | F  -- Bool, Char, Int, Float
  deriving (Eq, Read, Show)

$(addNont "Term")
$(addNont "Ty")

$(addProd "Var" ''Nt_Term
  [  ("var", Ter ''Variable) ])
$(addProd "Abs" ''Nt_Term
  [   ("absVar", Ter ''Variable),
      ("absVarType", NonTer ''Nt_Ty),
      ("absExpr", NonTer ''Nt_Term) ])
$(addProd "App" ''Nt_Term
  [  ("appFun", NonTer ''Nt_Term),
     ("appArg", NonTer ''Nt_Term)])

$(addProd "ConstTy" ''Nt_Ty
  [  ("const", Ter ''Tys) ])
$(addProd "AppTy" ''Nt_Ty
  [   ("appArgTy", NonTer ''Nt_Ty),
      ("appResTy", NonTer ''Nt_Ty) ])
$(closeNTs [''Nt_Ty, ''Nt_Term])

type Context = Map Variable Ty

$(attLabels [("eval", ''Ty), ("ctx", ''Context)])

eval_var = syndefM eval p_Var (slookup <$> ter ch_var <*> at lhs ctx)
  where slookup n m = fromJust $ Data.Map.lookup n m
eval_abs = syndefM eval p_Abs (AppTy <$> at ch_absVarType eval <*> at ch_absExpr eval)
eval_app = syndefM eval p_App (reduceApp <$> at ch_appFun eval <*> at ch_appArg eval)
  where reduceApp (AppTy argTy resTy) argTy2 = if argTy == argTy2 then resTy else error $ "El tipo " ++ show argTy ++ " no coincide con " ++ show argTy2
        reduceApp tyFun           tyArg      = error $ "Tipo inválido " ++ show tyFun ++ " en la aplicación a " ++ show tyArg

eval_constTy = syndefM eval p_ConstTy (ConstTy <$> ter ch_const)
eval_appTy   = syndefM eval p_AppTy (AppTy <$> at ch_appArgTy eval <*> at ch_appResTy eval)
asp_eval = eval_var .+: eval_abs .+: eval_app .+: eval_constTy .+: eval_appTy .+: emptyAspect

ctx_abs    = inhdefM ctx p_Abs ch_absExpr (insert <$> ter ch_absVar <*> at ch_absVarType eval <*> at lhs ctx)
ctx_appFun = inhdefM ctx p_App ch_appFun (at lhs ctx)
ctx_appArg = inhdefM ctx p_App ch_appArg (at lhs ctx)
asp_ctx = ctx_abs .+: ctx_appFun .+: ctx_appArg .+: emptyAspect

asp_all = asp_eval .:+: asp_ctx

$(mkSemFunc ''Nt_Ty)
$(mkSemFunc ''Nt_Term)

evalTerm term context = sem_Term asp_all term (ctx =. context .*. emptyAtt) #. eval

-- -- Ejemplos:

context = insert "x" (ConstTy B) $ insert "y" (ConstTy C) $ insert "z" (ConstTy I) empty

term1 = Var "x"
eval_term1 = evalTerm term1 context

term2 = Abs "y" (ConstTy C) (Var "x")
eval_term2 = evalTerm term2 context

term3 = App term2 (Var "y")
eval_term3 = evalTerm term3 context

term4 = App term1 term1
eval_term4 = evalTerm term4 context

term5 = Abs "w" (ConstTy F) (Var "w")
eval_term5 = evalTerm term5 context
