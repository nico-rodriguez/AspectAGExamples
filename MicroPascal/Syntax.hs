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

-- {-# OPTIONS_GHC -O0 #-}
-- {-# OPTIONS_GHC +RTS #-}
-- {-# OPTIONS_GHC -A128m #-}
-- {-# OPTIONS_GHC -n8m #-}
-- {-# OPTIONS_GHC -s #-}
-- {-# OPTIONS_GHC -RTS #-}
-- {-# OPTIONS_GHC -j2 #-}


module MicroPascal.Syntax where


import qualified Data.ByteString.Char8 as B
import Language.Grammars.AspectAG
import Language.Grammars.AspectAG.TH


type Name = String

data Type
  = TBool | TInt
  deriving (Show, Eq, Read)

data BOp
  = OOr | OAnd | OEq | OLT | OPlus | OMinus | OTimes | ODiv | OMod
  deriving (Show, Eq, Read)

data UOp
  = ONot | OOp
  deriving (Show, Eq, Read)

$(addNont "Program")
$(addNont "Defs"); $(addNont "DefList")
$(addNont "Body"); $(addNont "StmtList")
$(addNont "Stmt")
$(addNont "Expr")


$(addProd "Program" ''Nt_Program [("programName", Ter ''Name),
                                  ("programDefs", NonTer ''Nt_Defs),
                                  ("programBody", NonTer ''Nt_Body)])

$(addProd "EmptyDef" ''Nt_DefList [])
$(addProd "ConsDef" ''Nt_DefList  [("varName", Ter ''Name),
                                   ("varType", Ter ''Type),
                                   ("tailDefList", NonTer ''Nt_Defs)])
$(addProd "Defs" ''Nt_Defs [("defList", NonTer ''Nt_DefList)])


$(addProd "ConsStmt"    ''Nt_StmtList [("headStmt", NonTer ''Nt_Stmt),
                                       ("tailStmtList", NonTer ''Nt_StmtList)])
$(addProd "EmptyStmt"  ''Nt_StmtList [] )
$(addProd "Body" ''Nt_Body [("bodyStmts", NonTer ''Nt_StmtList)])


$(addProd "Assign" ''Nt_Stmt [("assignName", Ter ''Name),
                              ("assignExpr", NonTer ''Nt_Expr)])
$(addProd "If" ''Nt_Stmt [("ifCond", NonTer ''Nt_Expr),
                          ("ifThen", NonTer ''Nt_Body),
                          ("ifElse", NonTer ''Nt_Body)])
$(addProd "While" ''Nt_Stmt [("whileCond", NonTer ''Nt_Expr),
                             ("whileDo" , NonTer ''Nt_Body)])
$(addProd "WriteLn" ''Nt_Stmt [("writeLnExpr", NonTer ''Nt_Expr)])
$(addProd "ReadLn" ''Nt_Stmt [("readLnName", Ter ''Name)])

$(addProd "Var" ''Nt_Expr [("litName", Ter ''Name)])
$(addProd "Bool" ''Nt_Expr [("litBool", Ter ''Bool)])
$(addProd "NatL" ''Nt_Expr [("litNat", Ter ''Int)])
$(addProd "BOpExpr" ''Nt_Expr [("l", NonTer ''Nt_Expr),
                               ("bop", Ter ''BOp),
                               ("r", NonTer ''Nt_Expr)])
$(addProd "UOpExpr" ''Nt_Expr [("uop", Ter ''UOp),
                               ("e", NonTer ''Nt_Expr)])

$(closeNTs [''Nt_Program, ''Nt_Body, ''Nt_StmtList,
            ''Nt_DefList, ''Nt_Defs, ''Nt_Stmt, ''Nt_Expr])

$(mkSemFuncs [''Nt_Program, ''Nt_Body, ''Nt_StmtList,
            ''Nt_DefList, ''Nt_Defs, ''Nt_Stmt, ''Nt_Expr])


-- Example: pretty printing expressions

$(attLabel "sshow" ''B.ByteString)

sshow_Program     = syndefM sshow p_Program $ showProgram <$> ter ch_programName <*> at ch_programDefs sshow <*> at ch_programBody sshow
  where showProgram n d b = B.concat [B.pack "Program ", n, a, B.pack ";\n", d, b]
sshow_EmptyDef    = syndefM sshow p_EmptyDef $ pure B.empty
sshow_ConsDef     = syndefM sshow p_ConsDef $ showConsDef <$> ter ch_varName <*> (show <$> ter ch_varType) <*> at ch_tailDefList sshow
  where showConsDef n t d = B.concat [n, B.pack " : ", t, B.pack ";\n", d]
sshow_Defs        = syndefM sshow p_Defs $ showDefs <$> at ch_defList sshow
  where
    showDefs dl@ConsDef{} = B.append (B.pack "Var ") dl
    showDefs EmptyDef     = B.pack "Var\n"

sshow_ConsStmt    = syndefM sshow p_ConsStmt $ showConsStmt <$> at ch_headStmt sshow <*> at ch_tailStmtList sshow
  where showConsStmt h t = B.append h t
sshow_EmptyStmt  = syndefM sshow p_EmptyStmt $ pure B.empty
sshow_Body        = syndefM sshow p_Body $ showBody <$> at ch_bodyStmts sshow
  where showBody b = B.concat [B.pack "Begin\n", b, B.pack "End.\n"]
sshow_Assign      = syndefM sshow p_Assign $ showAssign <$> ter ch_assignName <*> at ch_assignExpr sshow
  where showAssign n e = B.concat [n, B.pack " := ", e, B.pack ";\n"]
sshow_If          = syndefM sshow p_If $ showIf <$> at ch_ifCond sshow <*> at ch_ifThen sshow <*> at ch_ifElse sshow
  where showIf c t e = B.concat [B.pack "If ", c, B.pack " then \nbegin\n", t, B.pack "end\nelse\nbegin\n", e, B.pack "end\n"]
sshow_While       = syndefM sshow p_While $ showWhile <$> at ch_whileCond sshow <*> at ch_whileDo sshow
  where showWhile c b = B.concat [B.pack "While ", c, B.pack " do\nbegin", b, B.pack "end\n"]
sshow_WriteLn     = syndefM sshow p_WriteLn $ showwriteLn <$> at ch_writeLnExpr sshow
  where showwriteLn e = B.concat [B.pack "WriteLn ", e, B.pack ";\n"]
sshow_ReadLn      = syndefM sshow p_ReadLn $ showReadLn <$> ter ch_readLnName
  where showReadLn n = B.concat [B.pack "ReadLn ", n, B.pack ";\n"]
sshow_Var         = syndefM sshow p_Var  $ B.pack (ter ch_litName)
sshow_Bool        = syndefM sshow p_Bool $ B.pack . show <$> ter ch_litBool
sshow_NatL        = syndefM sshow p_NatL $ B.pack . show <$> ter ch_litNat
sshow_BOpExpr     = syndefM sshow p_BOpExpr $ wrap <$> at ch_l sshow <*> (B.pack . show <$> ter ch_bop) <*> at ch_r sshow
  where wrap l op r = B.concat [B.pack "(", l, B.pack " ", op, B.pack " ", r, B.pack ")"]
sshow_UOpExpr     = syndefM sshow p_UOpExpr $ wrap <$> (B.pack . show <$> ter ch_uop) <*> at ch_e sshow
  where wrap op e = B.concat [B.pack "(", op, B.pack " ", e, B.pack ")"]

asp_sshow = sshow_Program .+: sshow_EmptyDef .+: sshow_ConsDef .+: sshow_Defs .+:
  sshow_ConsStmt .+: sshow_EmptyStmt .+: sshow_Body .+: sshow_Assign .+: sshow_If .+: sshow_While .+: sshow_WriteLn .+: sshow_ReadLn .+:
  sshow_Var .+: sshow_Bool .+: sshow_NatL .+: sshow_BOpExpr .+: sshow_UOpExpr .+: emptyAspect

showProgram :: Program -> IO ()
showProgram p = B.putStr $ (sem_Program asp_sshow p EmptyAtt) #. sshow

prog1 = Program "ejemplo1" (Defs EmptyDef) (Body EmptyStmt)
prog2 = Program "ejemplo2" (Defs (ConsDef "x" TInt (Defs (ConsDef "y" TInt (Defs (ConsDef "b" TBool (Defs EmptyDef)))))))
  (Body (ConsStmt (Assign "x" (NatL 10)) (
    ConsStmt (Assign "y" (BOpExpr (Var "x") OTimes (BOpExpr (NatL 3) OPlus (NatL 2)))) (
      ConsStmt (Assign "b" (Bool True)) (
        ConsStmt (Assign "b" (UOpExpr ONot (BOpExpr (Var "x") OLT (NatL 10))))
          EmptyStmt
        )
      )
    )
  ))

main :: IO ()
main = do
  putStrLn "*** Programa 1 ***\n"
  showProgram prog1
  putStrLn "*** Programa 2 ***\n"
  showProgram prog2
