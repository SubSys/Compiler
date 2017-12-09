{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module LLIR.LightRoast.AST.TermLevel.Stmt (
      IR.Stmt
    , pattern FunCall
    , pattern ConCall
    , pattern Ref
    , pattern Lit
    , pattern Tuple
    , pattern List
    , pattern Case
) where


-- *
import Core

--- Local
import qualified LLIR.LightRoast.Internal.AST as IR
-- *




pattern FunCall :: IR.Low -> [IR.Stmt] -> IR.Stmt
pattern FunCall name args = IR.FunCallStmt name args

pattern ConCall :: IR.Big -> [IR.Stmt] -> IR.Stmt
pattern ConCall name args = IR.ConCallStmt name args

pattern Ref :: IR.Low -> IR.Stmt
pattern Ref name = IR.RefStmt name

pattern Lit :: IR.LiteralValue -> IR.Stmt
pattern Lit val = IR.LitStmt val

pattern Tuple :: [IR.Stmt] -> IR.Stmt
pattern Tuple items = IR.TupleStmt items

pattern List :: [IR.Stmt] -> IR.Stmt
pattern List xs = IR.ListStmt xs

pattern Case :: IR.Stmt -> [IR.CaseAlt] -> IR.Stmt
pattern Case con alts = IR.CaseStmt con alts





















