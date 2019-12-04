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


module BoolExpr2 where

import Language.Grammars.AspectAG
import Language.Grammars.AspectAG.TH
import Prelude (Bool (True,False), (&&), (||), (<*>), (<$>), not)

-- Modelar expresiones booleanas con los operadores and y not.

$(addNont "BoolExpr")

$(addProd "Val" ''Nt_BoolExpr
  [  ("val", Ter ''Bool) ])
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

eval_and = syndefM eval_bool p_And ((&&) <$> at ch_leftAnd eval_bool <*> at ch_rightAnd eval_bool)
eval_or  = syndefM eval_bool p_Or ((||) <$> at ch_leftOr eval_bool <*> at ch_rightOr eval_bool)
eval_not = syndefM eval_bool p_Not (Prelude.not <$> at ch_not eval_bool)
eval_val = syndefM eval_bool p_Val (ter ch_val)
asp_eval_bool = eval_and .+: eval_or .+: eval_val .+: eval_not .+: emptyAspect

$(mkSemFunc ''Nt_BoolExpr)

evalBoolExpr expr = sem_BoolExpr asp_eval_bool expr emptyAtt #. eval_bool

--Ejemplos:

expr_bool = Or (And (Not (Val True)) (Val True)) (Not (Val False))
eval_expr_bool = evalBoolExpr expr_bool
