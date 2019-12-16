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


module MicroPascal.TypeChecker where


import Data.Maybe (fromJust)
import qualified Data.Map.Strict as M
import Language.Grammars.AspectAG
import Language.Grammars.AspectAG.TH
import MicroPascal.Syntax


data Error = Duplicated Name
           | Undefined  Name
           | Expected   Type Type
instance Show Error where
  show (Duplicated    n) = "Duplicated definition: " ++ n
  show (Undefined     n) = "Undefined: " ++ n
  show (Expected ty ty') = "Expected: " ++ show ty ++ " Actual: " ++ show ty'
type Errors = [Error]
 
type DeclVar = M.Map Name Type

$(attLabels [("errty", ''Errors), ("errname", ''Errors), ("decls", ''DeclVar), ("defs", ''DeclVar), ("ty", ''Type)])

errty_Program   = syndefM errty p_Program $ at ch_programBody errty
errty_ConsStmt  = syndefM errty p_ConsStmt $ (++) <$> at ch_headStmt errty <*> at ch_tailStmtList errty
errty_EmptyStmt = syndefM errty p_EmptyStmt $ pure []
errty_Body      = syndefM errty p_Body $ at ch_bodyStmts errty
errty_Assign    = syndefM errty p_Assign $ errAssign <$> ter ch_assignName <*> at ch_assignExpr ty <*> at lhs decls <*> at ch_assignExpr errty
  where errAssign n t m e = if fromJust (M.lookup n m) == t
          then e
          else (Expected t (fromJust (M.lookup n m))) : e
errty_If        = syndefM errty p_If $ errIf <$> at ch_ifCond ty <*> at ch_ifCond errty <*> at ch_ifThen errty <*> at ch_ifElse errty
  where errIf ct ce te ee = if ct == TBool
        then ce ++ te ++ ee
        else (Expected TBool ct) : ce ++ te ++ ee
errty_While     = syndefM errty p_While $ errWhile <$> at ch_whileCond ty <*> at ch_whileCond errty <*> at ch_whileDo errty
  where errWhile ct ce e = if ct == TBool
        then ce ++ e
        else (Expected TBool ct) : ce ++ e
errty_WriteLn   = syndefM errty p_WriteLn $ errWriteLn <$> at ch_writeLnExpr ty <*> at ch_writeLnExpr errty
  where errWriteLn t e = if t == TInt
        then e
        else (Expected TInt t) : e
errty_ReadLn    = syndefM errty p_ReadLn $ errReadLn <$> ter ch_readLnName <*> at lhs decls
  where errReadLn n m = if fromJust (M.lookup n m) == TInt 
          then []
          else [Expected TInt (fromJust (M.lookup n m))]
errty_Var       = syndefM errty p_Var $ pure []
errty_Bool      = syndefM errty p_Bool $ pure []
errty_NatL      = syndefM errty p_NatL $ pure []
errty_BOpExpr   = syndefM errty p_BOpExpr $ errBOpExpr <$> at ch_l errty <*> at ch_l ty <*> ter ch_bop <*> at ch_r errty <*> at ch_r ty
  where
    errBOpExpr le lt OOr re rt = if lt == TBool
        then if rt == TBool
          then le ++ re
          else (Expected TBool rt) : le ++ re
        else if rt == TBool
          then (Expected TBool lt) : le ++ re
          else (Expected TBool lt) : (Expected TBool rt) : le ++ re
    errBOpExpr le lt OAnd re rt = if lt == TBool
        then if rt == TBool
          then le ++ re
          else (Expected TBool rt) : le ++ re
        else if rt == TBool
          then (Expected TBool lt) : le ++ re
          else (Expected TBool lt) : (Expected TBool rt) : le ++ re
    errBOpExpr le lt OEq re rt = if lt == TInt
        then if rt == TInt
          then le ++ re
          else (Expected TInt rt) : le ++ re
        else if rt == TInt
          then (Expected TInt lt) : le ++ re
          else (Expected TInt lt) : (Expected TInt rt) : le ++ re
    errBOpExpr le lt OLT re rt = if lt == TInt
        then if rt == TInt
          then le ++ re
          else (Expected TInt rt) : le ++ re
        else if rt == TInt
          then (Expected TInt lt) : le ++ re
          else (Expected TInt lt) : (Expected TInt rt) : le ++ re
    errBOpExpr le lt OPlus re rt = if lt == TInt
        then if rt == TInt
          then le ++ re
          else (Expected TInt rt) : le ++ re
        else if rt == TInt
          then (Expected TInt lt) : le ++ re
          else (Expected TInt lt) : (Expected TInt rt) : le ++ re
    errBOpExpr le lt OMinus re rt = if lt == TInt
        then if rt == TInt
          then le ++ re
          else (Expected TInt rt) : le ++ re
        else if rt == TInt
          then (Expected TInt lt) : le ++ re
          else (Expected TInt lt) : (Expected TInt rt) : le ++ re
    errBOpExpr le lt OTimes re rt = if lt == TInt
        then if rt == TInt
          then le ++ re
          else (Expected TInt rt) : le ++ re
        else if rt == TInt
          then (Expected TInt lt) : le ++ re
          else (Expected TInt lt) : (Expected TInt rt) : le ++ re
    errBOpExpr le lt ODiv re rt = if lt == TInt
        then if rt == TInt
          then le ++ re
          else (Expected TInt rt) : le ++ re
        else if rt == TInt
          then (Expected TInt lt) : le ++ re
          else (Expected TInt lt) : (Expected TInt rt) : le ++ re
    errBOpExpr le lt OMod re rt = if lt == TInt
        then if rt == TInt
          then le ++ re
          else (Expected TInt rt) : le ++ re
        else if rt == TInt
          then (Expected TInt lt) : le ++ re
          else (Expected TInt lt) : (Expected TInt rt) : le ++ re
errty_UOpExpr   = syndefM errty p_UOpExpr $ errUOPExpr <$> ter ch_uop <*> at ch_e ty <*> at ch_e errty
  where
    errUOPExpr ONot t e = if t == TBool
      then e
      else (Expected TBool t) : e
    errUOPExpr OOp t e = if t == TInt
        then e
        else (Expected TInt t) : e

asp_errty = errty_Program .+: errty_ConsStmt .+: errty_EmptyStmt .+:
  errty_Body .+: errty_Assign .+: errty_If .+: errty_While .+: errty_WriteLn .+: errty_ReadLn .+:
  errty_Var .+: errty_Bool .+: errty_NatL .+: errty_BOpExpr .+: errty_UOpExpr .+: emptyAspect


errname_Program   = syndefM errname p_Program $ (++) <$> at ch_programDefs errname <*> at ch_programBody errname
errname_EmptyDef  = syndefM errname p_EmptyDef $ pure []
errname_ConsDef   = syndefM errname p_ConsDef $ errConsDef <$> ter ch_varName <*> at lhs decls <*> at ch_tailDefList errname
  where errConsDef n m d = if M.member n m
        then (Duplicated n) : d
        else d
errname_Defs      = syndefM errname p_Defs $ at ch_defList errname

errname_ConsStmt  = syndefM errname p_ConsStmt $ showConsStmt <$> at ch_headStmt errname <*> at ch_tailStmtList errname
  where showConsStmt h t = h ++ t
errname_EmptyStmt = syndefM errname p_EmptyStmt $ pure []
errname_Body      = syndefM errname p_Body $ at ch_bodyStmts errname
errname_Assign    = syndefM errname p_Assign $ errAssign <$> ter ch_assignName <*> at lhs decls <*> at ch_assignExpr errname
  where errAssign n m e = if M.member n m
        then e
        else (Undefined n) : e
errname_If        = syndefM errname p_If $ errIf <$> at ch_ifCond errname <*> at ch_ifThen errname <*> at ch_ifElse errname
  where errIf ce te ee = ce ++ te ++ ee
errname_While     = syndefM errname p_While $ (++) <$> at ch_whileCond errname <*> at ch_whileDo errname
errname_WriteLn   = syndefM errname p_WriteLn $ pure []
errname_ReadLn    = syndefM errname p_ReadLn $ errReadLn <$> ter ch_readLnName <*> at lhs decls
  where errReadLn n m = if M.member n m
        then []
        else [Undefined n]
errname_Var       = syndefM errname p_Var $ errVar <$> ter ch_litName <*> at lhs decls
  where errVar n m = if M.member n m
        then []
        else [Undefined n]
errname_Bool      = syndefM errname p_Bool $ pure []
errname_NatL      = syndefM errname p_NatL $ pure []
errname_BOpExpr   = syndefM errname p_BOpExpr $ (++) <$> at ch_l errname <*> at ch_r errname
errname_UOpExpr   = syndefM errname p_UOpExpr $ at ch_e errname

asp_errname = errname_Program .+: errname_EmptyDef .+: errname_ConsDef .+: errname_Defs .+: errname_ConsStmt .+: errname_EmptyStmt .+:
  errname_Body .+: errname_Assign .+: errname_If .+: errname_While .+: errname_WriteLn .+: errname_ReadLn .+:
  errname_Var .+: errname_Bool .+: errname_NatL .+: errname_BOpExpr .+: errname_UOpExpr .+: emptyAspect


defs_Program  = syndefM defs p_Program $ at ch_programDefs defs
defs_Defs     = syndefM defs p_Defs $ at ch_defList defs
defs_EmptyDef = syndefM defs p_EmptyDef $ pure (M.empty::DeclVar)
defs_ConsDef  = syndefM defs p_ConsDef $ defsConsDef <$> ter ch_varName <*> ter ch_varType <*> at ch_tailDefList defs
  where defsConsDef n t m = M.insert n t m

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

asp_decls = decls_programBody .+: decls_headStmt .+: tailStmtList .+:
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
