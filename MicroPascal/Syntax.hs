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


module MicroPascal.Syntax where


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


-- Simplest program
prog1 = Program "ejemplo1" (Defs EmptyDef) (Body EmptyStmt)

-- Variable declaration and assignment
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

-- If example
prog3 = Program "ejemplo3" (Defs (ConsDef "w" TInt (Defs (ConsDef "x" TBool (Defs EmptyDef)))))
  (Body (ConsStmt (ReadLn "w") (
    ConsStmt (Assign "x" (BOpExpr (Var "w") OLT (NatL 10))) (
      ConsStmt (If (Var "x") (Body (ConsStmt (WriteLn (Var "w")) EmptyStmt)) (Body (ConsStmt (WriteLn (NatL 10)) EmptyStmt))) EmptyStmt
      )
    )
  ))

-- While example
prog4 = Program "ejemplo4" (Defs (ConsDef "x" TInt (Defs EmptyDef)))
  (Body (ConsStmt (ReadLn "x") (
    ConsStmt (While (BOpExpr (NatL 0) OLT (Var "x")) (Body (ConsStmt (WriteLn (Var "x")) (ConsStmt (Assign "x" (BOpExpr (Var "x") OMinus (NatL 1))) EmptyStmt)))) EmptyStmt
    )
  ))

-- Duplicated definitions
prog5 = Program "ejemplo5" (Defs (ConsDef "x" TInt (Defs (ConsDef "y" TBool (Defs (ConsDef "y" TInt (Defs (ConsDef "y" TInt (Defs (ConsDef "x" TInt (Defs (ConsDef  "y" TBool (Defs EmptyDef)))))))))))))
  (Body (ConsStmt (ReadLn "y") (
    ConsStmt (Assign "x" (BOpExpr (Var "x") OPlus (Var "y"))) EmptyStmt
    )
  ))

-- Undefined names
prog6 = Program "ejemplo6" (Defs (ConsDef "x" TInt (Defs (ConsDef "y" TInt (Defs EmptyDef)))))
  (Body (ConsStmt (ReadLn "z") (
    ConsStmt (Assign "i" (NatL 1)) (
      ConsStmt (While (BOpExpr (Var "i") OLT (NatL 10)) (Body (ConsStmt (Assign "i" (BOpExpr (Var "i") OPlus (NatL 1))) EmptyStmt))) EmptyStmt
      )
    )
  ))

-- Type errors
prog7 = Program "ejemplo7" (Defs (ConsDef "b" TBool (Defs (ConsDef "x" TInt (Defs EmptyDef)))))
  (Body (ConsStmt (ReadLn "b") (
    ConsStmt (ReadLn "x") (
      ConsStmt (If (BOpExpr (Var "b") OAnd (Var "x")) (Body (ConsStmt (WriteLn (Var "x")) EmptyStmt)) (Body (ConsStmt (While (BOpExpr (Var "x") OTimes (NatL 4)) (Body (ConsStmt (WriteLn (Var "b")) EmptyStmt))) EmptyStmt))) (
        ConsStmt (Assign "x" (BOpExpr (BOpExpr (Var "x") OPlus (Bool True)) OAnd (BOpExpr (Var "b") OOr (NatL 8)))) EmptyStmt
      )
    )
  )))

-- Program to optimize
prog8 = Program "ejemplo8" (Defs (ConsDef "x" TInt (Defs EmptyDef)))
  (Body (ConsStmt (ReadLn "x") (
    ConsStmt (Assign "x" (BOpExpr (NatL 2) OPlus (BOpExpr (Var "x") OTimes (NatL 0)))) (
      ConsStmt (Assign "x" (BOpExpr (BOpExpr (NatL 3) OPlus (NatL 2)) OTimes (Var "x"))) (
        ConsStmt (Assign "x" (BOpExpr (Var "x") OPlus (BOpExpr (NatL 0) ODiv (NatL 3)))) (
          ConsStmt (If (BOpExpr (BOpExpr (Var "x") OEq (NatL 38)) OAnd (Bool False)) (Body (ConsStmt (WriteLn (Var "x")) EmptyStmt)) (Body (ConsStmt (WriteLn (BOpExpr (Var "x") OPlus (NatL 1))) EmptyStmt))) (
            ConsStmt (While (BOpExpr (BOpExpr (NatL 2) OEq (NatL 3)) OOr (Bool False)) (Body (ConsStmt (WriteLn (Var "x")) EmptyStmt))) EmptyStmt
          )
        )
      )
    )
  )))

-- Optimized program
prog8' = Program "ejemplo8'" (Defs (ConsDef "x" TInt (Defs EmptyDef)))
  (Body (ConsStmt (ReadLn "x") (
    ConsStmt (Assign "x" (NatL 2)) (
      ConsStmt (Assign "x" (BOpExpr (NatL 5) OTimes (Var "x"))) (
        ConsStmt (Assign "x" (Var "x")) (
          ConsStmt (WriteLn (BOpExpr (Var "x") OTimes (NatL 1))) EmptyStmt
        )
      )
    )
  )))
