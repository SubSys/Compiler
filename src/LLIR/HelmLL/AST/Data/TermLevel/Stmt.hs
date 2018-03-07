{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module LLIR.HelmLL.AST.Data.TermLevel.Stmt (
    IR.Stmt
  , IR.Block
  
  , pattern Block

  , pattern Ref
  , pattern Lit
  , pattern FunCall
  , pattern ConCall
  , pattern If
  , pattern Case
  , pattern List
  , pattern Tuple
  , pattern Loop
  
  , pattern If_
  , pattern Loop_
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

import qualified LLIR.HelmLL.Internal.AST as IR
-- ~


pattern Block :: [IR.Stmt] -> IR.Block
pattern Block stmts = IR.Block stmts


-- | Statements
--
pattern Ref :: IR.Ident -> IR.Stmt
pattern Ref name = IR.RefStmt (IR.Bounded name)

pattern Lit :: IR.LiteralValue -> IR.Stmt
pattern Lit val = IR.LitStmt val

pattern FunCall :: IR.Ident -> [IR.Stmt] -> IR.Stmt
pattern FunCall name args = IR.FunCallStmt (IR.Bounded name) args

pattern ConCall :: IR.Ident -> [IR.Stmt] -> IR.Stmt
pattern ConCall name args = IR.ConCallStmt name args

pattern If :: [(IR.Stmt, IR.Block)] -> IR.Block -> IR.Stmt
pattern If intros elseBlock = IR.IfStmt intros elseBlock

pattern Case :: IR.Stmt -> [IR.CaseAlt] -> IR.Stmt
pattern Case con alts = IR.CaseStmt con alts

pattern List :: [IR.Stmt] -> IR.Stmt
pattern List xs = IR.ListStmt xs

pattern Tuple :: [IR.Stmt] -> IR.Stmt
pattern Tuple items = IR.TupleStmt items

pattern Loop :: IR.Stmt -> IR.Block -> IR.Stmt
pattern Loop con body = IR.LoopStmt con body



-- | Alternate variations - convenience helpers
--


pattern If_ :: [(IR.Stmt, [IR.Stmt])] -> [IR.Stmt] -> IR.Stmt
pattern If_ intros elseBlock <- IR.IfStmt (unwrapBranches -> intros) (unwrap -> elseBlock)
    where
        If_ intros elseBlock =  IR.IfStmt (wrapBranches intros) (IR.Block elseBlock)

pattern Loop_ :: IR.Stmt -> [IR.Stmt] -> IR.Stmt
pattern Loop_ con body <- IR.LoopStmt con (unwrap -> body)
    where
        Loop_ con body = IR.LoopStmt con (IR.Block body)


-- | Internal Helpers
--

unwrap :: IR.Block -> [IR.Stmt]
unwrap (IR.Block stmts) = stmts

unwrapBranches :: [(IR.Stmt, IR.Block)] -> [(IR.Stmt, [IR.Stmt])]
unwrapBranches (List.unzip -> (cons, blocks)) =
    List.zip cons (map unwrap blocks)


wrapBranches :: [(IR.Stmt, [IR.Stmt])] -> [(IR.Stmt, IR.Block)]
wrapBranches (List.unzip -> (cons, blocks)) =
    List.zip cons (map IR.Block blocks)


