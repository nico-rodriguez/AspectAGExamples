{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}


module SystemF where

import Data.Map (Map, lookup, empty, insert)
import Data.Set (Set, member, empty, insert)
import Data.Maybe (fromJust)
import Language.Grammars.AspectAG
import Language.Grammars.AspectAG.TH
import Prelude (String, ($), (<*>), (<$>), Eq, Read, Show, show, (++), error, (==))

-- Modelar el cálculo lambda polimórifco (System F).
-- Lenguaje de términos:
-- t :: = x           (variables)
--     | \x^ty . t    (lambda abstracciones)
--     | t t          (aplicaciones)
--     | /\ a . t     (abstracción de tipo)
--     | t a          (aplicación de tipo)
-- Las dos últimas producciones son una extensión del cálculo lambda simplemente tipado.
-- Lenguaje de tipos:
-- ty ::=
--     | b            (constantes de tipo)
--     | a            (variables de tipo)
--     | ty -> ty
--     | \/ a . ty    (tipo universal)
-- b ::= bool | int | char | float
-- La primera producción es una generalización del cálculo lambda simplemente tipado; la última es una extensión.

type Variable = String
data Tys = B | C | I | F  -- Bool, Char, Int, Float
  deriving (Eq, Read, Show)
type TypeVariable = String

$(addNont "Term")
$(addNont "Ty")

$(addProd "Var" ''Nt_Term
  [  ("var", Ter ''Variable) ])
$(addProd "Abs" ''Nt_Term
  [   ("absVar", Ter ''Variable),
      ("absVarType", NonTer ''Nt_Ty),
      ("absExpr", NonTer ''Nt_Term) ])
$(addProd "App" ''Nt_Term
  [   ("appFun", NonTer ''Nt_Term),
      ("appArg", NonTer ''Nt_Term)])
$(addProd "TyAbs" ''Nt_Term
  [   ("tyAbsTyVar", Ter ''TypeVariable),
      ("tyAbsTerm", NonTer ''Nt_Term)])
$(addProd "TyApp" ''Nt_Term
  [   ("tyAppTerm", NonTer ''Nt_Term),
      ("tyAppTy", NonTer ''Nt_Ty)])

$(addProd "ConstTy" ''Nt_Ty
  [   ("constTy", Ter ''Tys)])
$(addProd "VarTy" ''Nt_Ty
  [   ("varTy", Ter ''TypeVariable) ])
$(addProd "AppTy" ''Nt_Ty
  [   ("appArgTy", NonTer ''Nt_Ty),
      ("appResTy", NonTer ''Nt_Ty) ])
$(addProd "UnivTy" ''Nt_Ty
  [   ("univTyVar", Ter ''TypeVariable),
      ("univTyTy", NonTer  ''Nt_Ty)])
$(closeNTs [''Nt_Ty, ''Nt_Term])

type Context = Map Variable Ty
type ContextTypes = Set TypeVariable

$(attLabels [("eval", ''Ty), ("ctx", ''Context), ("ctx_var", ''ContextTypes), ("sshow", ''String)])

eval_var = syndefM eval p_Var (slookup <$> ter ch_var <*> at lhs ctx)
  where slookup n m = fromJust $ Data.Map.lookup n m
eval_abs = syndefM eval p_Abs (AppTy <$> at ch_absVarType eval <*> at ch_absExpr eval)
eval_app = syndefM eval p_App (reduceApp <$> at ch_appFun eval <*> at ch_appArg eval)
  where reduceApp (AppTy argTy resTy) argTy2 = if argTy == argTy2 then resTy else error $ "El tipo " ++ show argTy ++ " no coincide con " ++ show argTy2
        reduceApp tyFun           tyArg      = error $ "Tipo inválido " ++ show tyFun ++ " en la aplicación a " ++ show tyArg
eval_tyAbs = syndefM eval p_TyAbs (UnivTy <$> ter ch_tyAbsTyVar <*> at ch_tyAbsTerm eval)
eval_tyApp = syndefM eval p_TyApp (replace <$> at ch_tyAppTerm eval <*> at ch_tyAppTy eval)
  where replace (UnivTy tyVar ty) ty2 = replace' ty tyVar ty2
        replace _                 _   = error $ "Tipo inválido para la abstracción de tipo"
        replace' t@(VarTy tyVar')      tyVar ty2 = if tyVar' == tyVar then ty2 else t
        replace' (AppTy argTy resTy)   tyVar ty2 = AppTy (replace' argTy tyVar ty2) (replace' resTy tyVar ty2)
        replace' t@(UnivTy tyVar' ty') tyVar ty2 = if tyVar' == tyVar then t else UnivTy tyVar' (replace' ty' tyVar ty2)
        replace' t@(ConstTy _)        _     _   = t

eval_constTy = syndefM eval p_ConstTy (ConstTy <$> ter ch_constTy)
eval_varTy   = syndefM eval p_VarTy (checkVarTy <$> ter ch_varTy <*> at lhs ctx_var)
  where checkVarTy varTy ctx_Var =
          if Data.Set.member varTy ctx_Var
          then VarTy varTy
          else error $ "La variable de tipo " ++ show varTy ++ " no se encuentra en el contexto " ++ show ctx_Var
eval_appTy   = syndefM eval p_AppTy (AppTy <$> at ch_appArgTy eval <*> at ch_appResTy eval)
eval_univTy  = syndefM eval p_UnivTy (UnivTy <$> ter ch_univTyVar <*> at ch_univTyTy eval)
asp_eval = eval_var .+: eval_abs .+: eval_app .+: eval_tyAbs .+: eval_tyApp .+: eval_constTy .+: eval_varTy .+: eval_appTy .+: eval_univTy .+: emptyAspect

ctx_abs    = inhdefM ctx p_Abs ch_absExpr (Data.Map.insert <$> ter ch_absVar <*> at ch_absVarType eval <*> at lhs ctx)
ctx_appFun = inhdefM ctx p_App ch_appFun (at lhs ctx)
ctx_appArg = inhdefM ctx p_App ch_appArg (at lhs ctx)
ctx_tyAbs  = inhdefM ctx p_TyAbs ch_tyAbsTerm (at lhs ctx)
ctx_tyApp  = inhdefM ctx p_TyApp ch_tyAppTerm (at lhs ctx)
asp_ctx = ctx_abs .+: ctx_appFun .+: ctx_appArg .+: ctx_tyAbs .+: ctx_tyApp .+: emptyAspect

ctx_var_abs    = inhdefM ctx_var p_Abs ch_absExpr (at lhs ctx_var)
ctx_var_absVarType = inhdefM ctx_var p_Abs ch_absVarType (at lhs ctx_var)
ctx_var_appFun = inhdefM ctx_var p_App ch_appFun (at lhs ctx_var)
ctx_var_appArg = inhdefM ctx_var p_App ch_appArg (at lhs ctx_var)
ctx_var_tyAbs  = inhdefM ctx_var p_TyAbs ch_tyAbsTerm (Data.Set.insert <$> ter ch_tyAbsTyVar <*> at lhs ctx_var)
ctx_var_tyApp  = inhdefM ctx_var p_TyApp ch_tyAppTerm (at lhs ctx_var)
ctx_var_tyAppTy = inhdefM ctx_var p_TyApp ch_tyAppTy (at lhs ctx_var)

ctx_var_appArgTy  = inhdefM ctx_var p_AppTy ch_appArgTy (at lhs ctx_var)
ctx_var_appResTy  = inhdefM ctx_var p_AppTy ch_appResTy (at lhs ctx_var)
ctx_var_univTyTy  = inhdefM ctx_var p_UnivTy ch_univTyTy (at lhs ctx_var)
asp_ctx_var = ctx_var_abs .+: ctx_var_absVarType .+: ctx_var_appFun .+: ctx_var_appArg .+: ctx_var_tyAbs .+: ctx_var_tyApp .+: ctx_var_tyAppTy .+:
  ctx_var_appArgTy .+: ctx_var_appResTy .+: ctx_var_univTyTy .+: emptyAspect


arrow = " -> "-- '→'
forAll = "V "--'∀'
absType = "A "--'∧'
dot = " . "
sshow_var = syndefM sshow p_Var (ter ch_var)
sshow_abs = syndefM sshow p_Abs (showAbs <$> ter ch_absVar <*> at ch_absVarType sshow <*> at ch_absExpr sshow)
  where showAbs sAbsVar sAvsVarType sAbsExpr = sAbsVar ++ "^" ++ sAvsVarType ++ arrow ++ sAbsExpr
sshow_app = syndefM sshow p_App (showApp <$> at ch_appFun sshow <*> at ch_appArg sshow)
  where showApp sAppFun sAppArg = "(" ++ sAppFun ++ ") " ++ sAppArg
sshow_tyAbs = syndefM sshow p_TyAbs (showTyAbs <$> ter ch_tyAbsTyVar <*> at ch_tyAbsTerm sshow)
  where showTyAbs sTyAbsTyVar sTyAbsTerm = absType ++ sTyAbsTyVar ++ dot ++ sTyAbsTerm
sshow_tyApp = syndefM sshow p_TyApp (showTyApp <$> at ch_tyAppTerm sshow <*> at ch_tyAppTy sshow)
  where showTyApp sTyAppTerm sTyAppTy = "(" ++ sTyAppTerm ++ ") " ++ sTyAppTy

sshow_constTy = syndefM sshow p_ConstTy (show <$> ter ch_constTy)
sshow_varTy   = syndefM sshow p_VarTy (ter ch_varTy)
sshow_appTy   = syndefM sshow p_AppTy (showAppTy <$> at ch_appArgTy sshow <*> at ch_appResTy sshow)
  where showAppTy sAppArgTy sAppResTy = sAppArgTy ++ arrow ++ sAppResTy
sshow_univTy  = syndefM sshow p_UnivTy (showUnivTy <$> ter ch_univTyVar <*> at ch_univTyTy sshow)
  where showUnivTy sUnivTyVar sUnivTyTy = forAll ++ sUnivTyVar ++ dot ++ sUnivTyTy
asp_type_sshow = sshow_constTy .+: sshow_varTy .+: sshow_appTy .+: sshow_univTy .+: emptyAspect
asp_sshow = sshow_var .+: sshow_abs .+: sshow_app .+: sshow_tyAbs .+: sshow_tyApp .+: asp_type_sshow

asp_all = asp_eval .:+: asp_ctx_var .:+: asp_ctx

$(mkSemFunc ''Nt_Ty)
$(mkSemFunc ''Nt_Term)

evalTerm term context = sem_Term asp_all term ( (ctx =. context) .*. (ctx_var =. (Data.Set.empty::ContextTypes)) .*. emptyAtt ) #. eval

showTerm term = (sem_Term asp_sshow term emptyAtt #. sshow)
showType term context = sem_Ty asp_type_sshow (evalTerm term context) emptyAtt #. sshow
showTermAndType term context = (showTerm term) ++ " : " ++ (showType term context)

-- Ejemplos:

context = Data.Map.insert "x" (VarTy "a") $ Data.Map.insert "y" (VarTy "b") $ Data.Map.insert "z" (VarTy "c") Data.Map.empty

term1 = Var "x"
eval_term1 = evalTerm term1 context

term2 = Abs "y" (VarTy "b") (Var "x")
eval_term2 = evalTerm term2 context

term3 = App term2 (Var "y")
eval_term3 = evalTerm term3 context

term4 = App term1 term1
eval_term4 = evalTerm term4 context

term5 = Abs "w" (VarTy "d") (Var "w")
eval_term5 = evalTerm term5 context

term6 = TyAbs "alfa" (Abs "x" (VarTy "beta") (Var "x"))
eval_term6 = evalTerm term6 (Data.Map.empty)

term7 = TyAbs "alfa" (Abs "x" (VarTy "alfa") (Var "x"))
eval_term7 = evalTerm term7 (Data.Map.empty)

term8 = TyApp term7 (ConstTy I)
eval_term8 = evalTerm term8 (Data.Map.empty)
