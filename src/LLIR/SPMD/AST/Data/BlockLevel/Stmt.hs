{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module LLIR.SPMD.AST.Data.BlockLevel.Stmt (
    IR.Block
  , IR.Stmt
  
  , pattern Block

  , pattern Assignment
  , pattern Ref
  , pattern ConCall
  , pattern FunCall
  , pattern MethodAccess
  , pattern ArrayAccess
  , pattern Lit
  , pattern If
  , pattern Switch
  , pattern For
  , pattern While
  , pattern Continue
  , pattern Break
  , pattern Return
  , pattern Discard
  , pattern Init
  
  -- , pattern If'
  -- , pattern If_
) where


-- ~
import Core
import Core.Utils as Core
import Core.List.Util (singleton)

import Prelude (error, ($))

import qualified Data.List    as List
import qualified Data.Maybe   as Maybe
import qualified Data.Text    as Text
import qualified Data.String  as String

import qualified LLIR.SPMD.Internal.AST as IR
-- ~



-- | Statement Blocks
--

pattern Block :: [IR.Stmt] -> IR.Block
pattern Block stmts = IR.Block stmts


-- | Expression Statements
--

pattern Assignment :: IR.Stmt -> IR.Stmt -> IR.Stmt
pattern Assignment lvalue rvalue = IR.ExprStmt (IR.AssignmentStmt lvalue rvalue)

pattern Ref :: IR.Ident -> IR.Stmt
pattern Ref ident = IR.ExprStmt (IR.ReferenceStmt ident)

pattern ConCall :: IR.Ident -> [IR.Stmt] -> IR.Stmt
pattern ConCall name args = IR.ExprStmt (IR.ConstructorCallStmt name args)

pattern FunCall :: IR.Ident -> [IR.Stmt] -> IR.Stmt
pattern FunCall name args = IR.ExprStmt (IR.FunctionCallStmt name args)

pattern MethodAccess :: IR.Ident -> IR.Ident -> IR.Stmt
pattern MethodAccess name method = IR.ExprStmt (IR.MethodAccessStmt name method)

pattern ArrayAccess :: IR.Ident -> IR.Index -> IR.Stmt
pattern ArrayAccess name index = IR.ExprStmt (IR.ArrayAccessStmt name index)

pattern Lit :: IR.LiteralValue -> IR.Stmt
pattern Lit value = IR.ExprStmt (IR.LiteralStmt value)


-- | Selection Statements
--

pattern If :: [(IR.Stmt, IR.Block)] -> Maybe IR.Block -> IR.Stmt
pattern If intros elseBranch = IR.SelectionStmt (IR.IfStmt intros elseBranch)

pattern Switch :: IR.Stmt -> [(IR.Stmt, IR.Block)] -> Maybe IR.Block -> IR.Stmt
pattern Switch con branches defaultBranch = IR.SelectionStmt (IR.SwitchStmt con branches defaultBranch)



-- | Iteration Statements
--

pattern For :: (IR.Stmt, IR.Stmt, IR.Stmt) -> IR.Block -> IR.Stmt
pattern For con body = IR.IterationStmt (IR.ForStmt con body)

pattern While :: IR.Stmt -> IR.Block -> IR.Stmt
pattern While con body = IR.IterationStmt (IR.WhileStmt con body)



-- | Jump Statements
--

pattern Continue :: IR.Stmt
pattern Continue = IR.JumpStmt IR.ContinueStmt

pattern Break :: IR.Stmt
pattern Break = IR.JumpStmt IR.BreakStmt

pattern Return :: Maybe IR.Stmt -> IR.Stmt
pattern Return value = IR.JumpStmt (IR.ReturnStmt value)

pattern Discard :: IR.Stmt
pattern Discard = IR.JumpStmt IR.DiscardStmt



-- | Init Local Object Statement
--

pattern Init :: IR.Type -> IR.Ident -> Maybe IR.Stmt -> IR.Stmt
pattern Init ty name constant = IR.InitLocalObject (IR.LocalObjectStmt ty name constant)








-- | Alternate variations - convenience helpers
--

-- pattern If' :: [(IR.Stmt, IR.Block)] -> IR.Stmt
-- pattern If' intros = IR.SelectionStmt (IR.IfStmt intros Nothing)


pattern If_ :: [(IR.Stmt, [IR.Stmt])] -> Maybe [IR.Stmt] -> IR.Stmt
pattern If_ intros elseBranch <- IR.SelectionStmt (IR.IfStmt (extractIFs -> intros) (extractDefault -> elseBranch))
    where
        If_ intros elseBranch = IR.SelectionStmt (IR.IfStmt (wrapIFs intros) (wrapDefault elseBranch))

-- pattern Switch_ :: IR.Stmt -> [(IR.Stmt, [IR.Stmt])] -> Maybe [IR.Stmt] -> IR.Stmt
-- pattern Switch_ con branches defaultBranch = IR.SelectionStmt (IR.SwitchStmt con branches defaultBranch)
--
-- pattern For_ :: (IR.Stmt, IR.Stmt, IR.Stmt) -> IR.Block -> IR.Stmt
-- pattern For_ con body = IR.IterationStmt (IR.ForStmt con body)
-- 
-- pattern While_ :: IR.Stmt -> IR.Block -> IR.Stmt
-- pattern While_ con body = IR.IterationStmt (IR.WhileStmt con body)


-- | Internal Helpers
--


extract :: IR.Block -> [IR.Stmt]
extract (IR.Block stmts) = stmts

extractIFs :: [(IR.Stmt, IR.Block)] -> [(IR.Stmt, [IR.Stmt])]
extractIFs (List.unzip -> (cases, map extract -> body)) =
    List.zip cases body

extractDefault :: Maybe IR.Block -> Maybe [IR.Stmt]
extractDefault = Core.applyMaybe extract


wrapIFs :: [(IR.Stmt, [IR.Stmt])] -> [(IR.Stmt, IR.Block)]
wrapIFs (List.unzip -> (cases, body)) =
    List.zip cases (map IR.Block body)

wrapDefault :: Maybe [IR.Stmt] -> Maybe IR.Block
wrapDefault = Core.applyMaybe IR.Block


