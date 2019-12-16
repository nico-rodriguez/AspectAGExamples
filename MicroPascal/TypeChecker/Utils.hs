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


module MicroPascal.TypeChecker.Utils where


import Data.Maybe (fromJust)
import qualified Data.Map.Strict as M
import Language.Grammars.AspectAG
import Language.Grammars.AspectAG.TH
import MicroPascal.Syntax


type DeclVar = M.Map Name Type

$(attLabels [("defs", ''DeclVar), ("decls", ''DeclVar), ("ty", ''Type)])


defs_Program  = syndefM defs p_Program $ at ch_programDefs defs
defs_Defs     = syndefM defs p_Defs $ at ch_defList defs
defs_EmptyDef = syndefM defs p_EmptyDef $ pure (M.empty::DeclVar)
defs_ConsDef  = syndefM defs p_ConsDef $ M.insert <$> ter ch_varName <*> ter ch_varType <*> at ch_tailDefList defs

asp_defs = defs_Program .+: defs_Defs .+: defs_EmptyDef .+: defs_ConsDef .+: emptyAspect


decls_programBody  = inhdefM decls p_Program ch_programBody $ at lhs defs
decls_headStmt     = inhdefM decls p_ConsStmt ch_headStmt $ at lhs decls
decls_tailStmtList = inhdefM decls p_ConsStmt ch_tailStmtList $ at lhs decls
decls_bodyStmts    = inhdefM decls p_Body ch_bodyStmts $ at lhs decls
decls_assignExpr   = inhdefM decls p_Assign ch_assignExpr $ at lhs decls
decls_ifCond       = inhdefM decls p_If ch_ifCond $ at lhs decls
decls_ifThen       = inhdefM decls p_If ch_ifThen $ at lhs decls
decls_ifElse       = inhdefM decls p_If ch_ifElse $ at lhs decls
decls_whileCond    = inhdefM decls p_While ch_whileCond $ at lhs decls
decls_whileDo      = inhdefM decls p_While ch_whileDo $ at lhs decls
decls_writeLnExpr  = inhdefM decls p_WriteLn ch_writeLnExpr $ at lhs decls
decls_l            = inhdefM decls p_BOpExpr ch_l $ at lhs decls
decls_r            = inhdefM decls p_BOpExpr ch_r $ at lhs decls
decls_e            = inhdefM decls p_UOpExpr ch_e $ at lhs decls

asp_decls = decls_programBody .+: decls_headStmt .+: decls_tailStmtList .+:
  decls_bodyStmts .+: decls_assignExpr .+: decls_ifCond .+: decls_ifThen .+: decls_ifElse .+: decls_whileCond .+: decls_whileDo .+: decls_writeLnExpr .+:
  decls_l .+: decls_r .+: decls_e .+: emptyAspect


ty_Var         = syndefM ty p_Var $ tyVar <$> ter ch_litName <*> at lhs decls
  where tyVar n m = fromJust $ M.lookup n m
ty_Bool        = syndefM ty p_Bool $ pure TBool
ty_NatL        = syndefM ty p_NatL $ pure TInt
ty_BOpExpr     = syndefM ty p_BOpExpr $ tyBOpExpr <$> ter ch_bop
  where
    tyBOpExpr OOr    = TBool
    tyBOpExpr OAnd   = TBool
    tyBOpExpr OEq    = TBool
    tyBOpExpr OLT    = TBool
    tyBOpExpr OPlus  = TInt
    tyBOpExpr OMinus = TInt
    tyBOpExpr OTimes = TInt
    tyBOpExpr ODiv   = TInt
    tyBOpExpr OMod   = TInt
ty_UOpExpr     = syndefM ty p_UOpExpr $ tyUOPExpr <$> ter ch_uop
  where
    tyUOPExpr ONot = TBool
    tyUOPExpr OOp  = TInt

asp_ty = ty_Var .+: ty_Bool .+: ty_NatL .+: ty_BOpExpr .+: ty_UOpExpr .+: emptyAspect