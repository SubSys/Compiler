{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module CGIR.RustCG.AST.Data.Semantic.BlockLevel.Stmt (
    IR.Stmt
  , IR.Block(..)
  
  , pattern Box
  , pattern Lit
  , pattern Ref
  , pattern FunCall
  , pattern ConCall
  , pattern If
  , pattern Match
  , pattern List
  , pattern Tuple
) where


-- *
import Core

--- Local
import qualified CGIR.RustCG.Internal.AST as IR
-- *




pattern Box :: IR.Stmt -> IR.Stmt
pattern Box value = IR.BoxStmt value

pattern Lit :: IR.LiteralValue -> IR.Stmt
pattern Lit val = IR.LitStmt val

pattern Ref :: IR.Path -> IR.Stmt
pattern Ref path = IR.RefStmt path

pattern FunCall :: IR.Path -> [IR.Stmt] -> IR.Stmt
pattern FunCall path args = IR.FunCallStmt path args

pattern ConCall :: IR.Path -> [IR.Stmt] -> IR.Stmt
pattern ConCall path args = IR.ConCallStmt path args

pattern If :: [(IR.Stmt, IR.Stmt)] -> IR.Stmt -> IR.Stmt
pattern If intros elseStmt = IR.IfStmt intros elseStmt

pattern Match :: IR.Stmt -> [IR.Arm] -> IR.Stmt
pattern Match con arms = IR.MatchStmt con arms

pattern List :: [IR.Stmt] -> IR.Stmt
pattern List xs = IR.ListStmt xs

pattern Tuple :: [IR.Stmt] -> IR.Stmt
pattern Tuple items = IR.TupleStmt items
