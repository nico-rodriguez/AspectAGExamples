{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}


module Instr where

import Data.Map (Map, empty, insert, union)
import Language.Grammars.AspectAG
import Language.Grammars.AspectAG.TH
import Prelude (Bool (True,False), String, (<*>), (<$>), pure, foldl)
import BoolExpr2

--Modelar un mini lenguaje imperativo con expresiones booleanas

-- <Instr> := Skip
--     | Var := BoolExpr
--     | IF BE Then <Instr> Else <Instr>
--     | Begin <LInstr> End
-- <LInstr> := NilI
--     | <Instr>; <LInstr>

$(addNont "Instr")
$(addNont "LInstr")
$(addProd "Skip" ''Nt_Instr
  [  ])
$(addProd "VarI" ''Nt_Instr
  [ ("name", Ter ''String),
    ("expr", NonTer ''Nt_BoolExpr) ])
$(addProd "If" ''Nt_Instr
  [ ("cond" , NonTer ''Nt_BoolExpr),
    ("thenI", NonTer ''Nt_Instr),
    ("elseI", NonTer ''Nt_Instr)   ])
$(addProd "Begin" ''Nt_Instr
  [  ("linstr", NonTer ''Nt_LInstr)])
$(addProd "BeginNil" ''Nt_LInstr
  [  ])
$(addProd "BeginCons" ''Nt_LInstr
  [ ("headI" , NonTer ''Nt_Instr),
    ("tailLI", NonTer ''Nt_LInstr)  ])
$(closeNTs [''Nt_LInstr, ''Nt_Instr])

-- Los programas de este lenguaje se interpretan como funciones que modifican la "memoria". Se entiende la memoria como
-- una función que asigna un valor booleano a cada variable, representada como un string.
type Memory  = Map String Bool
type LMemory = [Map String Bool] 
$(attLabels [("env", ''Memory), ("lenv", ''LMemory)])

env_skip      = syndefM env p_Skip (pure (empty::Memory))
env_var       = syndefM env p_VarI (insert <$> ter ch_name <*> at ch_expr eval_bool <*> pure (empty::Memory))
env_if        = syndefM env p_If (cond <$> at ch_cond eval_bool <*> at ch_thenI env <*> at ch_elseI env)
  where cond c t e = if c then t else e
env_begin     = syndefM env p_Begin (foldl union (empty::Memory) <$> at ch_linstr lenv)
env_beginNil  = syndefM lenv p_BeginNil (pure [])
env_beginCons = syndefM lenv p_BeginCons ((:) <$> at ch_headI env <*> at ch_tailLI lenv)
asp_env = env_skip .+: env_var .+: env_if .+: env_begin .+: env_beginNil .+: env_beginCons .+: emptyAspect

asp_all = asp_env .:+: asp_eval_bool

$(mkSemFunc ''Nt_LInstr)
$(mkSemFunc ''Nt_Instr)

evalInstr instr = sem_Instr asp_all instr (env =. (empty :: Memory) .*. emptyAtt) #. env

-- Ejemplos:

instr1 = Skip
eval_instr1 = evalInstr instr1

instr2 = VarI "x" (Or (And (Not (Val True)) (Val True)) (Not (Val False)))
eval_instr2 = evalInstr instr2
