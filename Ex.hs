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


module Ex where

import Data.Map (Map, lookup, insert)
import Data.Set (Set, member, empty)
import Data.Maybe (fromJust)
import Language.Grammars.AspectAG
import Language.Grammars.AspectAG.TH
import Prelude (String, ($), (<*>), (<$>), show, (++), error)


type Variable = String
type TypeVariable = String

$(addNont "A")
$(addNont "B")

$(addProd "Var" ''Nt_A
  [  ("var", Ter ''Variable) ])
$(addProd "Abs" ''Nt_A
  [   ("absVar", Ter ''Variable),
      ("absVarBpe", NonTer ''Nt_B),
      ("absExpr", NonTer ''Nt_A) ])

$(addProd "VarB" ''Nt_B
  [   ("varB", Ter ''TypeVariable) ])
$(addProd "AppB" ''Nt_B
  [   ("appArgB", NonTer ''Nt_B),
      ("appResB", NonTer ''Nt_B) ])
$(closeNTs [''Nt_B, ''Nt_A])

type Context = Map Variable B
type ContextTypes = Set TypeVariable

$(attLabels [("eval", ''B), ("ctx", ''Context), ("ctx_var", ''ContextTypes), ("sshow", ''String)])

eval_var   = syndefM eval p_Var (slookup <$> ter ch_var <*> at lhs ctx)
  where slookup n m = fromJust $ Data.Map.lookup n m
eval_abs   = syndefM eval p_Abs (AppB <$> at ch_absVarBpe eval <*> at ch_absExpr eval)
eval_varB = syndefM eval p_VarB (checkVarB <$> ter ch_varB <*> at lhs ctx_var)
  where checkVarB varB ctx_Var =
          if Data.Set.member varB ctx_Var
          then VarB varB
          else error $ "La variable de tipo " ++ show varB ++ " no se encuentra en el contexto " ++ show ctx_Var
eval_appB   = syndefM eval p_AppB (AppB <$> at ch_appArgB eval <*> at ch_appResB eval)
asp_eval = eval_var .+: eval_abs .+: eval_varB.+: eval_appB .+: emptyAspect

ctx_abs    = inhdefM ctx p_Abs ch_absExpr (Data.Map.insert <$> ter ch_absVar <*> at ch_absVarBpe eval <*> at lhs ctx)
asp_ctx = ctx_abs .+: emptyAspect

ctx_var_abs        = inhdefM ctx_var p_Abs ch_absExpr (at lhs ctx_var)
ctx_var_absVarBpe = inhdefM ctx_var p_Abs ch_absVarBpe (at lhs ctx_var)
ctx_var_appArgB   = inhdefM ctx_var p_AppB ch_appArgB (at lhs ctx_var)
ctx_var_appResB   = inhdefM ctx_var p_AppB ch_appResB (at lhs ctx_var)
asp_ctx_var = ctx_var_abs .+: ctx_var_absVarBpe .+: ctx_var_appArgB .+: ctx_var_appResB .+: emptyAspect

asp_all = asp_eval .:+: asp_ctx_var .:+: asp_ctx

$(mkSemFunc ''Nt_B)
$(mkSemFunc ''Nt_A)

evalA term context = sem_A asp_all term ( (ctx =. context) .*. (ctx_var =. (Data.Set.empty::ContextTypes)) .*. emptyAtt ) #. eval
