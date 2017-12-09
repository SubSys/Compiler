{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module CGIR.Rust.AST.BlockLevel.Stmts (
      pattern FnCall
    , pattern FnCall'
    , pattern MethodCall
    , pattern ImplCall
    , pattern Variant
    , pattern Struct
    , pattern Tuple
    , pattern Scalar
    , pattern LetDecl
    , pattern SelfRef
    , pattern VarRef
    , pattern SelfVarRef
    , pattern PathRef
    , pattern Assign
    , pattern Block
    , pattern FnDecl
    , pattern Match
    , IR.Stmt
    , StmtPath
) where


-- *
import Core
import Data.Data (Data, Typeable)

--- Local
import qualified CGIR.Rust.Internal.AST        as IR
-- *




-- *
-- | Call-Site Callers
-- *


pattern FnCall :: StmtPath -> IR.Stmt
pattern FnCall path =
    IR.FnCallStmt path

pattern FnCall' :: IR.Low -> [IR.Stmt] -> IR.Stmt
pattern FnCall' name args =
    IR.FnCallStmt
        (IR.Path [ IR.LowSeg name [] Nothing args ])


pattern MethodCall :: IR.Low -> [IR.Stmt] -> IR.Stmt
pattern MethodCall name args = IR.MethodCallStmt (IR.MethodChain name args)

pattern ImplCall :: StmtPath -> [IR.MethodChain] -> IR.Stmt
pattern ImplCall path callees = IR.ImplCallStmt path callees



-- *
-- | Value Constructors
-- *

-- | Complex
pattern Variant :: StmtPath -> IR.Stmt
pattern Variant path = IR.VariantStmt path

pattern Struct :: StmtPath ->  [(IR.Low, IR.Stmt)] -> IR.Stmt
pattern Struct path fields = IR.StructStmt path fields

pattern Tuple :: [IR.Stmt] -> IR.Stmt
pattern Tuple xs = IR.TupleStmt xs


-- | Scalars
pattern Scalar :: IR.ScalarValue -> IR.Stmt
pattern Scalar val = IR.ScalarStmt val



-- *
-- | Local Declarations
-- *
pattern LetDecl :: IR.Low -> IR.Stmt -> IR.Stmt
pattern LetDecl var bind = IR.LetStmt var bind



-- | Statement-Level Features
--
pattern Block :: [IR.Stmt] -> IR.Stmt
pattern Block stmts = IR.BlockStmt (IR.Block stmts)


pattern FnDecl :: IR.FunctionDecl -> IR.Stmt
pattern FnDecl fnDecl = IR.FnStmt fnDecl



-- | Control-Flow Constructs
--
pattern Match :: IR.Stmt -> [IR.Arm] -> IR.Stmt
pattern Match stmt arms = IR.MatchStmt stmt arms



-- *
-- | Etc.
-- *
pattern SelfRef :: IR.Stmt
pattern SelfRef = IR.SelfRefStmt Nothing

pattern SelfVarRef :: Maybe IR.Low -> IR.Stmt
pattern SelfVarRef var = IR.SelfRefStmt var

pattern VarRef :: IR.Low -> IR.Stmt
pattern VarRef name = IR.VarRefStmt name


pattern PathRef :: StmtPath -> IR.Stmt
pattern PathRef path = IR.PathRefStmt path



pattern Assign :: IR.Stmt -> IR.Stmt -> IR.Stmt
pattern Assign lv rv = IR.AssignStmt lv rv






-- *
-- | Misc.
-- *
type StmtPath = IR.Path IR.Stmt





