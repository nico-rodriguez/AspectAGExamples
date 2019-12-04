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


module Expr where

import Prelude (Int, Bool (True), String, Show, Read, Eq, (<*>), (<$>), (++))
import Data.Map (Map, empty, insert, map)
import Language.Grammars.AspectAG
import Language.Grammars.AspectAG.TH
import BoolExpr
import IntExpr

--expr -> bool_expr | int_expr

--Definir la sintaxis del lenguaje

$(addNont "Expr")
$(addProd "BoolExpr" ''Nt_Expr [("bool_expr", NonTer ''Nt_BoolExpr)])
$(addProd "IntExpr"  ''Nt_Expr [("int_expr", NonTer ''Nt_IntExpr)])
$(closeNTs [''Nt_Expr])

--Definir la semántica del lenguaje

data Res = RB Bool | RI Int
  deriving (Eq, Read, Show)

$(attLabels [("eval", ''Res)])
env  = Label @ ('Att "env"  (Map String Res))

eval_bool_expr = syndefM eval p_BoolExpr (RB <$> at ch_bool_expr eval_bool)
eval_int_expr  = syndefM eval p_IntExpr (RI <$> at ch_int_expr eval_int)
asp_eval = eval_bool_expr .+: eval_int_expr .+: emptyAspect

env_bool_expr = inh env_bool p_BoolExpr ch_bool_expr (map (\(RB b) -> b) <$> at lhs env)
env_int_expr  = inh env_int p_IntExpr  ch_int_expr  (map (\(RI i) -> i) <$> at lhs env)
asp_env   = env_bool_expr .+: env_int_expr .+: emptyAspect

asp_all = asp_eval .:+: asp_env .:+: asp_eval_int .:+: asp_env_int .:+: asp_eval_bool .:+: asp_env_bool

$(mkSemFunc ''Nt_Expr)

evalExpr exp m = sem_Expr asp_all exp (env =. m .*. emptyAtt) #. eval

expr1 = BoolExpr expr_bool ::Expr
eval_expr1 = evalExpr expr1 (insert "x" (RB True) empty) :: Res

expr2 = IntExpr expr_int ::Expr
eval_expr2 = evalExpr expr2 (insert "x" (RI 5) empty) :: Res
