> {-# LANGUAGE TemplateHaskell #-}
> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE TypeSynonymInstances #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE TypeApplications #-}
> {-# LANGUAGE NoMonomorphismRestriction #-}


> module Resumen where

> import Prelude (Int, String, ($), return, (+), Show, (<*>), (<$>), (++), pure)
> import Data.Map (Map, lookup, empty, insert)
> import Data.Maybe (fromJust)
> import Data.Proxy
> import Language.Grammars.AspectAG
> import Language.Grammars.AspectAG.TH
> import GHC.TypeLits

Attribute Grammars Fly First-class... Safer!

ABSTRACT
- AspectAG es un DSL embebido en Haskell (EDSL) para representar gramáticas de atributos
- Permite definiciones modulares de las gramáticas
- Utiliza extensible records

1 INTRODUCTION
- Las AGs surgieron para representar semántica en gramáticas libres de contexto (GLC)
- Los atributos están asociados a las producciones
- Los valores de los atributos se computan localmente en cada nodo a partir de:
    - reglas semánticas
    - valores de atributos del padre
    - valores de atributos de los hijos
- Dos tipos de atributos:
    - sintetizados: son el resultado de la evaluacioens de las reglas semánticas (info. viaja hacia arriba)
    - heredados: info. viaja hacia abajo

2 OVERVIEW OF THE LIBRARY
- Ejemplo de lenguaje de valores enteros, variables y adición

expr -> ival                # valor entero
expr -> vname               # nombre de variable (string)
expr -> expr_l + expr_r

Definir la sintaxis del lenguaje

> $(addNont "Expr")
> $(addProd "Val" ''Nt_Expr [("ival", Ter ''Int)])
> $(addProd "Var" ''Nt_Expr [("vname", Ter ''String)])
> $(addProd "Add" ''Nt_Expr [("leftAdd", NonTer ''Nt_Expr), ("rightAdd", NonTer ''Nt_Expr)])

-- $(closeNTs [''Nt_Expr])

Definir la semántica del lenguaje

> $(attLabels [("eval", ''Int)])
> env  = Label @ ('Att "env"  (Map String Int))


> eval_add = syndefM eval p_Add ((+) <$> at ch_leftAdd eval <*> at ch_rightAdd eval)
> eval_var = syndefM eval p_Var (slookup <$> ter ch_vname <*> at lhs env)
>               where slookup n m = fromJust $ lookup n m
> eval_val = syn eval p_Val (ter ch_ival)

O utilizando la interfaz monádica:
eval_add = syn eval p_Add (do l <- at ch_leftAdd eval
                              r <- at ch_rightAdd eval
                              return (l + r))

eval_var = syn eval p_Var (do e <- at lhs env
                              x <- ter ch_vname
                              return (fromJust $ lookup x e))

> asp_eval = eval_add .+: eval_val .+: eval_var .+: emptyAspect

> env_add_l = inh env p_Add ch_leftAdd (at lhs env)
> env_add_r = inh env p_Add ch_rightAdd (at lhs env)

> asp_env = env_add_l .+: env_add_r .+: emptyAspect

> asp_all = asp_eval .:+: asp_env

-- $(mkSemFunc ''Nt_Expr)

-- evalExpr exp m = sem_Expr asp_all exp (env =. m .*. emptyAtt) #. eval

Ejemplos

-- expr1 = Add (Val (-9)) (Add (Var "x") (Val 2))

-- eval1 = evalExpr expr1 (insert "x" 5 Data.Map.empty)

2.1 Semantic Extension: Adding and Modifying attributes

Agregar el atributo lits

> lits  = Label @ ('Att "lits"  [Int])

> asp_lits = syndefM lits p_Add ((++) <$> at ch_leftAdd lits <*> at ch_rightAdd lits) .+: syndefM lits p_Val ((:[]) <$> ter ch_ival) .+:
>                syndefM lits p_Var (pure []) .+: emptyAspect

-- litsExpr expr = sem_Expr asp_lits expr emptyAtt #. lits

litsExpr expr1 = [-9,2]

Modificar la semántica de lits

> asp_lits_rev = synmodM lits p_Add ((++) <$> at ch_rightAdd lits <*> at ch_leftAdd lits) .+: asp_lits

-- litsExprRev expr = sem_Expr asp_lits_rev expr emptyAtt #. lits

litsExprRev expr1 = [2,-9]

2.2 Grammar Extension: Adding Productions

Se agrega la producción
expr -> let vname = expr_d in expr_i

> $(addProd "Let" ''Nt_Expr [("vlet", Ter ''String), ("exprLet", NonTer ''Nt_Expr), ("bodyLet", NonTer ''Nt_Expr)])
> $(closeNTs [''Nt_Expr])

Se extiende los aspectos definidos antes con la nueva producción

> asp_eval2 = {-traceAspect (Proxy @ ('Text "eval2")) $-} syndefM eval p_Let (at ch_bodyLet eval) .+: asp_eval
> asp_env2 = {-traceAspect (Proxy @ ('Text "env2")) $-} inhdefM env p_Let ch_exprLet (at lhs env) .+:
>    inhdefM env p_Let ch_bodyLet (insert <$> ter ch_vlet <*> at ch_exprLet eval <*> at lhs env) .+: asp_env
> asp_all2 = asp_eval2 .:+: asp_env2

> $(mkSemFunc ''Nt_Expr)

> evalExpr2 exp m = sem_Expr asp_all2 exp (env =. m .*. emptyAtt) #. eval

> expr2 = Add (Add (Val (-9)) (Let "z" (Val 1) (Add (Var "z") (Val 1)))) (Add (Var "x") (Val 2))

> eval2 = evalExpr2 expr2 (insert "x" 5 Data.Map.empty)