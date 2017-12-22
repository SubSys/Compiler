{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
module SLIR.HelmSyntax.Core.Program.SDD.Syntax.TopLevel.Functions (
    inferDecl
) where


-- *
import Core
import Core.Control.Flow ((|>), (<|))
import Core.List.Util    (flatten)
import Data.Data (Data, Typeable)

import Prelude (return, String, IO, show, error, (<$>), (>>))

import Data.List.Index  (imap)

import qualified Control.Monad.State        as M
import qualified Control.Monad.Except       as M
import qualified Control.Monad.RWS          as M
import qualified Control.Monad.Identity     as M
import qualified Control.Monad.Reader       as M
import qualified Control.Monad.Writer       as M

import qualified Data.List     as List
import qualified Data.Text     as Text
import qualified Data.Map      as Map
import qualified Data.Set      as Set
import qualified Data.Foldable as Fold
import qualified Data.Monoid   as Monoid

import qualified Data.Generics.Uniplate.Data as Uni
import qualified Text.Show.Prettyprint as PP


-- ~ HelmSyntax Cores

-- ~ HelmSyntax IR
import qualified SLIR.HelmSyntax.Data.Interface.Program.Payload as Payload

--- Local Deps
-- ~ HelmSyntax AST
-- ~~ Base
import qualified SLIR.HelmSyntax.AST.Data.Base.Etc      as Etc
import qualified SLIR.HelmSyntax.AST.Data.Base.Ident    as ID
import qualified SLIR.HelmSyntax.AST.Data.Base.Types    as T
import qualified SLIR.HelmSyntax.AST.Data.Base.Values   as V
import qualified SLIR.HelmSyntax.AST.Data.Base.Metadata as Meta

-- ~~ TermLevel
import qualified SLIR.HelmSyntax.AST.Data.TermLevel.Expressions as E
import qualified SLIR.HelmSyntax.AST.Data.TermLevel.Patterns    as P

-- ~~ TopLevel
import qualified SLIR.HelmSyntax.AST.Data.TopLevel.Fixities  as Decl
import qualified SLIR.HelmSyntax.AST.Data.TopLevel.Functions as Decl
import qualified SLIR.HelmSyntax.AST.Data.TopLevel.Unions    as Decl

-- ~~ Auxiliary AST - Nodes & Utils
import qualified SLIR.HelmSyntax.AST.Auxiliary.Canonical.Ident            as CID
import qualified SLIR.HelmSyntax.AST.Toolbox.SudoFFI                      as SudoFFI
import qualified SLIR.HelmSyntax.AST.Toolbox.TopLevel.Functions.Recursive as Rec


--- Local Prelude
import SLIR.HelmSyntax.Core.Program.SDD.Inference.Utils.Syntax.Base (enter, binder)


--- Local
import qualified SLIR.HelmSyntax.Core.Program.SDD.Inference.Data.Env               as Env
import qualified SLIR.HelmSyntax.Core.Program.SDD.Data.Report                      as Report
import qualified SLIR.HelmSyntax.Core.Program.SDD.Inference.Data.System            as Sys
import qualified SLIR.HelmSyntax.Core.Program.SDD.Inference.Utils.Syntax.Scope     as Scope
import qualified SLIR.HelmSyntax.Core.Program.SDD.Inference.Utils.TypeSystem       as TS
import qualified SLIR.HelmSyntax.Core.Program.SDD.Inference.Utils.Syntax.Constrain as Con
import qualified SLIR.HelmSyntax.Core.Program.SDD.Inference.Utils.Auxiliary        as Aux

-- ~ Sub Inferes
import qualified SLIR.HelmSyntax.Core.Program.SDD.Syntax.TermLevel.Expr as E
-- *




{-# ANN module "HLint: ignore" #-}





inferDecl :: Decl.Function -> Sys.Syntax Decl.Function
inferDecl fn@(SudoFFI.ifSudoDeclGetTy -> (Just (Left  ty))) = do
    initialEnv <- M.ask
    -- *

    -- *
    let scheme = TS.closeOver ty
    t         <- TS.instantiate scheme
    -- *

    -- *
    let newEnv = Map.insert (CID.ident fn) (Env.Normal scheme) initialEnv
    -- *
    
    
    -- *
    binder fn t initialEnv


inferDecl fn@(SudoFFI.ifSudoDeclGetTy -> (Just (Right  scheme))) = do
    initialEnv <- M.ask
    -- *

    -- *
    t  <- TS.instantiate $ TS.normalize scheme
    -- *

    -- *
    let newEnv = Map.insert (CID.ident fn) (Env.Normal (TS.normalize scheme)) initialEnv
    -- *
    
    
    -- *
    binder fn t initialEnv


inferDecl fn@(Rec.isRecDecl -> True) = inferRecDecl fn


inferDecl (Decl.FnDecl name args expr sig meta) = do
    initialEnv <- M.ask
    (tv, scheme) <- TS.freshTSPair
    -- *
    
    -- * Infer Expr
    (args', ts, exprEnv) <- Aux.inferList inferArg args
    (expr', et, _)       <- Scope.withLocalEnv exprEnv (E.inferExpr inferDecl expr)
    let t1 = Fold.foldr T.Arr' et ts
    -- *
    
    -- *
    Con.unify tv t1
    Con.unifySignature t1 sig
    -- *
    
    -- *
    let newEnv = Map.insert (CID.ident name) (Env.Normal (TS.normalize scheme)) initialEnv
    -- *
    
    
    
    -- *
    binder (Decl.FnDecl name args' expr' sig meta) tv newEnv



inferDecl (Decl.OpDecl sym args expr sig meta) = do
    initialEnv <- M.ask
    (tv, scheme) <- TS.freshTSPair
    
    -- * Infer Expr
    (args', ts, exprEnv) <- Aux.inferList inferArg args
    (expr', et, _)       <- Scope.withLocalEnv exprEnv (E.inferExpr inferDecl expr)
    let t1 = Fold.foldr T.Arr' et ts
    -- *
    
    -- *
    Con.unify tv t1
    Con.unifySignature t1 sig
    -- *
    
    -- *
    let newEnv = Map.insert (CID.ident sym) (Env.Normal scheme) initialEnv
    -- *
    
    -- *
    binder (Decl.OpDecl sym args' expr' sig meta) tv newEnv





-- *
-- | Internal Helpers
-- *


inferRecDecl :: Decl.Function -> Sys.Syntax Decl.Function
inferRecDecl (Decl.FnDecl name args expr sig meta) = do
    initialEnv <- M.ask
    -- *
    
    -- *
    (args', ts, env) <- Aux.inferList inferArg (name : args)
    (expr', et, _)   <- Scope.withLocalEnv env (E.inferExpr inferDecl expr)
    -- *
    
    -- *
    let (T.Arr' t1 t2) = Fold.foldr T.Arr' et ts
    -- *
    
    -- *
    tv0 <- TS.freshType
    -- *
    
    -- *
    Con.unify t1 t2
    Con.unify (tv0 `T.Arr'` tv0) t2
    -- *
    
    -- *
    Con.unifySignature t1 sig
    -- *
    
    -- *
    (returnType, scheme) <- TS.freshTSPair
    Con.unify t1 returnType
    -- *
    
    -- *
    let newEnv = Map.insert (CID.ident name) (Env.Normal scheme) initialEnv
    -- *
    
    binder (Decl.FnDecl name args expr' sig meta) returnType newEnv








inferArg :: ID.Low -> Sys.Syntax ID.Low
inferArg name = do
    env <- M.ask
    (tv, scheme) <- TS.freshTSPair

    let env' = Map.insert (CID.ident name) (Env.Normal scheme) env

    binder name tv env'






