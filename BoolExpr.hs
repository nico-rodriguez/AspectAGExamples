{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}


module BoolExpr where

import Data.Map (Map, lookup, empty, insert)
import Data.Maybe (fromJust)
import Language.Grammars.AspectAG
import Language.Grammars.AspectAG.TH
import Prelude (Bool (True,False), (&&), (||), String, ($), (<*>), (<$>), not)

-- Modelar expresiones booleanas con los operadores and y not, y con variables (strings).

$(addNont "BoolExpr")
nt_BoolExpr :: Label ('NT "BoolExpr")

$(addProd "Val" ''Nt_BoolExpr
  [  ("val", Ter ''Bool) ])
$(addProd "Var" ''Nt_BoolExpr
  [  ("var", Ter ''String) ])
$(addProd "And" ''Nt_BoolExpr
  [  ("leftAnd",   NonTer ''Nt_BoolExpr),
     ("rightAnd",  NonTer ''Nt_BoolExpr)])
$(addProd "Or" ''Nt_BoolExpr
  [  ("leftOr",   NonTer ''Nt_BoolExpr),
     ("rightOr",  NonTer ''Nt_BoolExpr)])
$(addProd "Not" ''Nt_BoolExpr
  [  ("not", NonTer ''Nt_BoolExpr)])
$(closeNTs [''Nt_BoolExpr])

$(attLabels [("eval_bool", ''Bool)])

env_bool  = Label @ ('Att "env_bool"  (Map String Bool))
eval_and = syndefM eval_bool p_And ((&&) <$> at ch_leftAnd eval_bool <*> at ch_rightAnd eval_bool)
eval_or  = syndefM eval_bool p_Or ((||) <$> at ch_leftOr eval_bool <*> at ch_rightOr eval_bool)
eval_not = syndefM eval_bool p_Not (Prelude.not <$> at ch_not eval_bool)
eval_val = syn eval_bool p_Val (ter ch_val)
eval_var = syndefM eval_bool p_Var (slookup <$> ter ch_var <*> at lhs env_bool)
            where slookup n m = fromJust $ Data.Map.lookup n m
asp_eval_bool = eval_and .+: eval_or .+: eval_val .+: eval_not .+: eval_var .+: emptyAspect

env_and_l = inh env_bool p_And ch_leftAnd (at lhs env_bool)
env_and_r = inh env_bool p_And ch_rightAnd (at lhs env_bool)
env_or_l  = inh env_bool p_Or ch_leftOr (at lhs env_bool)
env_or_r  = inh env_bool p_Or ch_rightOr (at lhs env_bool)
env_not   = inh env_bool p_Not ch_not (at lhs env_bool)
asp_env_bool = env_and_l .+: env_and_r .+: env_or_l .+: env_or_r .+: env_not .+: emptyAspect

asp_all_bool = asp_eval_bool .:+: asp_env_bool

$(mkSemFunc ''Nt_BoolExpr)

evalBoolExpr expr mem = sem_BoolExpr asp_all_bool expr (env_bool =. mem .*. emptyAtt) #. eval_bool

-- Ejemplos:

expr_bool = Or (And (Not (Val True)) (Var "x")) (Not (Val False))
eval_expr_bool = evalBoolExpr expr_bool (insert "x" True empty)
