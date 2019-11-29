> {-# LANGUAGE TemplateHaskell #-}
> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE TypeSynonymInstances #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE TypeApplications #-}
> {-# LANGUAGE NoMonomorphismRestriction #-}


> module IntExpr where

> import Prelude (Int, String, ($), (+), (<*>), (<$>))
> import Data.Map (Map, lookup, empty, insert)
> import Data.Maybe (fromJust)
> import Language.Grammars.AspectAG
> import Language.Grammars.AspectAG.TH

expr -> ival                # valor entero
expr -> vname               # nombre de variable (string)
expr -> expr_l + expr_r
expr -> let vname = expr_d in expr_i

Definir la sintaxis del lenguaje

> $(addNont "IntExpr")
> $(addProd "Val" ''Nt_IntExpr [("ival", Ter ''Int)])
> $(addProd "Var" ''Nt_IntExpr [("vname", Ter ''String)])
> $(addProd "Add" ''Nt_IntExpr [("leftAdd", NonTer ''Nt_IntExpr), ("rightAdd", NonTer ''Nt_IntExpr)])
> $(addProd "Let" ''Nt_IntExpr [("vlet", Ter ''String), ("exprLet", NonTer ''Nt_IntExpr), ("bodyLet", NonTer ''Nt_IntExpr)])
> $(closeNTs [''Nt_IntExpr])

Definir la semántica del lenguaje

> $(attLabels [("eval_int", ''Int)])
> env_int  = Label @ ('Att "env_int"  (Map String Int))

> eval_add = syndefM eval_int p_Add ((+) <$> at ch_leftAdd eval_int <*> at ch_rightAdd eval_int)
> eval_var = syndefM eval_int p_Var (slookup <$> ter ch_vname <*> at lhs env_int)
>               where slookup n m = fromJust $ lookup n m
> eval_val = syn eval_int p_Val (ter ch_ival)
> eval_let = syndefM eval_int p_Let (at ch_bodyLet eval_int)
> asp_eval_int = eval_add .+: eval_val .+: eval_var .+: eval_let .+: emptyAspect

> env_add_l = inh env_int p_Add ch_leftAdd (at lhs env_int)
> env_add_r = inh env_int p_Add ch_rightAdd (at lhs env_int)
> env_let_e = inhdefM env_int p_Let ch_exprLet (at lhs env_int)
> env_let_b = inhdefM env_int p_Let ch_bodyLet (insert <$> ter ch_vlet <*> at ch_exprLet eval_int <*> at lhs env_int)
> asp_env_int = env_add_l .+: env_add_r .+: env_let_e .+: env_let_b .+: emptyAspect

> asp_all_int = asp_eval_int .:+: asp_env_int

> $(mkSemFunc ''Nt_IntExpr)

> evalIntExpr exp m = sem_IntExpr asp_all_int exp (env_int =. m .*. emptyAtt) #. eval_int

> expr_int = Add (Add (Val (-9)) (Let "z" (Val 1) (Add (Var "z") (Val 1)))) (Add (Var "x") (Val 2))

> eval_expr_int = evalIntExpr expr_int (insert "x" 5 Data.Map.empty)
