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

$(attLabels [("err", ''Errors), ("decls", ''DeclVar), ("ty", ''Type)])

err_Program     = syndefM err p_Program $ (++) <$> at ch_programDefs err <*> at ch_programBody err
err_EmptyDef    = syndefM err p_EmptyDef $ pure []
err_ConsDef     = syndefM err p_ConsDef $ errConsDef <$> ter ch_varName <*> at lhs decls <*> at ch_tailDefList err
  where errConsDef n m d = if M.member n m
        then (Duplicated n) : d
        else d
err_Defs        = syndefM err p_Defs $ at ch_defList err

err_ConsStmt    = syndefM err p_ConsStmt $ showConsStmt <$> at ch_headStmt err <*> at ch_tailStmtList err
  where showConsStmt h t = h ++ t
err_EmptyStmt  = syndefM err p_EmptyStmt $ pure []
err_Body        = syndefM err p_Body $ at ch_bodyStmts err
err_Assign      = syndefM err p_Assign $ errAssign <$> ter ch_assignName <*> at ch_assignExpr ty <*> at lhs decls <*> at ch_assignExpr err
  where errAssign n t m e = if M.member n m
        then if fromJust (M.lookup n m) == t
          then e
          else [(Expected t (fromJust (M.lookup n m)))]
        else (Undefined n) : e
err_If          = syndefM err p_If $ errIf <$> at ch_ifCond ty <*> at ch_ifThen err <*> at ch_ifElse err
  where errIf ct te ee = if ct == TBool
        then te ++ ee
        else (Expected TBool ct) : te ++ ee
err_While       = syndefM err p_While $ errWhile <$> at ch_whileCond ty <*> at ch_whileDo err
  where errWhile ct e = if ct == TBool
        then e
        else (Expected TBool ct) : e
err_WriteLn     = syndefM err p_WriteLn $ errWriteLn <$> at ch_writeLnExpr ty <*> at ch_writeLnExpr err
  where errWriteLn t e = if t == TInt
        then e
        else (Expected TInt t) : e
err_ReadLn      = syndefM err p_ReadLn $ errReadLn <$> ter ch_readLnName <*> at lhs decls
  where errReadLn n m = if M.member n m
        then if fromJust (M.lookup n m) == TInt 
          then []
          else [Expected TInt (fromJust (M.lookup n m))]
        else
          [Undefined n]
err_Var         = syndefM err p_Var $ errVar <$> ter ch_litName <*> at lhs decls
  where errVar n m = if M.member n m
        then []
        else [Undefined n]
err_Bool        = syndefM err p_Bool $ pure []
err_NatL        = syndefM err p_NatL $ pure []
err_BOpExpr     = syndefM err p_BOpExpr $ errBOpExpr <$> at ch_l err <*> at ch_l ty <*> ter ch_bop <*> at ch_r err <*> at ch_r ty
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
err_UOpExpr     = syndefM err p_UOpExpr $ errUOPExpr <$> ter ch_uop <*> at ch_e ty <*> at ch_e err
  where
    errUOPExpr ONot t e = if t == TBool
      then e
      else (Expected TBool t) : e
    errUOPExpr OOp t e = if t == TInt
        then e
        else (Expected TInt t) : e
