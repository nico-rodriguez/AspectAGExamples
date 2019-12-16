{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE EmptyDataDeriving #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}


module MicroPascal.Optimizer where


import Language.Grammars.AspectAG
import Language.Grammars.AspectAG.TH
import MicroPascal.Syntax


$(attLabels [("optim_program", ''Program), ("optim_defs", ''Defs), ("optim_defList", ''DefList), ("optim_body", ''Body), ("optim_StmtList", ''StmtList), ("optim_Stmt", ''Stmt), ("optim_expr", ''Expr)])

optim_Program     = syndefM optim_program p_Program $ Program <$> ter ch_programName <*> at ch_programDefs optim_defs <*> at ch_programBody optim_body
optim_EmptyDef    = syndefM optim_defList p_EmptyDef $ pure EmptyDef
optim_ConsDef     = syndefM optim_defList p_ConsDef $ ConsDef <$> ter ch_varName <*> ter ch_varType <*> at ch_tailDefList optim_defs
optim_Defs        = syndefM optim_defs p_Defs $ Defs <$> at ch_defList optim_defList
optim_ConsStmt    = syndefM optim_StmtList p_ConsStmt $ ConsStmt <$> at ch_headStmt optim_Stmt <*> at ch_tailStmtList optim_StmtList
optim_EmptyStmt   = syndefM optim_StmtList p_EmptyStmt $ pure EmptyStmt
optim_Body        = syndefM optim_body p_Body $ Body <$> at ch_bodyStmts optim_StmtList
optim_Assign      = syndefM optim_Stmt p_Assign $ Assign <$> ter ch_assignName <*> at ch_assignExpr optim_expr
optim_If          = syndefM optim_Stmt p_If $ If <$> at ch_ifCond optim_expr <*> at ch_ifThen optim_body <*> at ch_ifElse optim_body
optim_While       = syndefM optim_Stmt p_While $ While <$> at ch_whileCond optim_expr <*> at ch_whileDo optim_body
optim_WriteLn     = syndefM optim_Stmt p_WriteLn $ WriteLn <$> at ch_writeLnExpr optim_expr
optim_ReadLn      = syndefM optim_Stmt p_ReadLn $ ReadLn <$> ter ch_readLnName
optim_Var         = syndefM optim_expr p_Var  $ Var <$> ter ch_litName
optim_Bool        = syndefM optim_expr p_Bool $ Bool <$> ter ch_litBool
optim_NatL        = syndefM optim_expr p_NatL $ NatL <$> ter ch_litNat
optim_BOpExpr     = syndefM optim_expr p_BOpExpr $ optimBOpExpr <$> at ch_l optim_expr <*> ter ch_bop <*> at ch_r optim_expr
  where
    optimBOpExpr (Bool True)  OOr _ = (Bool True)
    optimBOpExpr (Bool False) OOr b = b
    optimBOpExpr _ OOr (Bool True)  = (Bool True)
    optimBOpExpr b OOr (Bool False) = b
    optimBOpExpr b1 OOr b2 = BOpExpr b1 OOr b2

    optimBOpExpr (Bool True)  OAnd b = b
    optimBOpExpr (Bool False) OAnd _ = Bool False
    optimBOpExpr b OAnd (Bool True)  = b
    optimBOpExpr _ OAnd (Bool False) = Bool False
    optimBOpExpr b1 OAnd b2 = BOpExpr b1 OAnd b2

    optimBOpExpr (NatL n1) OEq (NatL n2) = Bool (n1 == n2)
    optimBOpExpr p1 OEq p2 = BOpExpr p1 OEq p2

    optimBOpExpr (NatL n1) OLT (NatL n2) = Bool (n1 < n2)
    optimBOpExpr p1 OLT p2 = BOpExpr p1 OLT p2

    optimBOpExpr (NatL 0) OPlus (NatL n2) = NatL n2
    optimBOpExpr (NatL n1) OPlus (NatL 0) = NatL n1
    optimBOpExpr (NatL n1) OPlus (NatL n2) = NatL (n1 + n2)
    optimBOpExpr p1 OPlus p2 = BOpExpr p1 OPlus p2

    optimBOpExpr (NatL 0) OMinus (NatL n2) = NatL (-n2)
    optimBOpExpr (NatL n1) OMinus (NatL 0) = NatL n1
    optimBOpExpr (NatL n1) OMinus (NatL n2) = NatL (n1 - n2)
    optimBOpExpr p1 OMinus p2 = BOpExpr p1 OMinus p2

    optimBOpExpr (NatL 0) OTimes (NatL _) = NatL 0
    optimBOpExpr (NatL _) OTimes (NatL 0) = NatL 0
    optimBOpExpr (NatL n1) OTimes (NatL n2) = NatL (n1 * n2)
    optimBOpExpr p1 OTimes p2 = BOpExpr p1 OTimes p2

    optimBOpExpr (NatL 0) ODiv (NatL _) = NatL 0
    optimBOpExpr (NatL n1) ODiv (NatL n2) = NatL (n1 `div` n2)
    optimBOpExpr p1 ODiv p2 = BOpExpr p1 ODiv p2

    optimBOpExpr (NatL 0) OMod (NatL _) = NatL 0
    optimBOpExpr (NatL n1) OMod (NatL n2) = NatL (n1 `mod` n2)
    optimBOpExpr p1 OMod p2 = BOpExpr p1 OMod p2
optim_UOpExpr     = syndefM optim_expr p_UOpExpr $ optimUOpExpr <$> ter ch_uop <*> at ch_e optim_expr
  where
    optimUOpExpr ONot (Bool b)    = Bool (not b)
    optimUOpExpr ONot e@UOpExpr{} = UOpExpr ONot e
    optimUOpExpr ONot e@BOpExpr{} = UOpExpr ONot e
    optimUOpExpr ONot e@NatL{}    = UOpExpr ONot e
    optimUOpExpr ONot e@Var{}     = UOpExpr ONot e

    optimUOpExpr OOp  (NatL n)    = NatL (-n)
    optimUOpExpr OOp  e@UOpExpr{} = UOpExpr OOp e
    optimUOpExpr OOp  e@BOpExpr{} = UOpExpr OOp e
    optimUOpExpr OOp  e@Bool{}    = UOpExpr OOp e
    optimUOpExpr OOp  e@Var{}     = UOpExpr OOp e

asp_optim = optim_Program .+: optim_EmptyDef .+: optim_ConsDef .+: optim_Defs .+:
  optim_ConsStmt .+: optim_EmptyStmt .+: optim_Body .+: optim_Assign .+: optim_If .+: optim_While .+: optim_WriteLn .+: optim_ReadLn .+:
  optim_Var .+: optim_Bool .+: optim_NatL .+: optim_BOpExpr .+: optim_UOpExpr .+: emptyAspect

optimizeProgram :: Program -> Program
optimizeProgram p = (sem_Program asp_optim p EmptyAtt) #. optim_program

main :: IO ()
main = do
  putStrLn "*** Programa 8 ***\n"
  putStr . show $ optimizeProgram prog8
