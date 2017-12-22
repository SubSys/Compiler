{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
module SLIR.HelmSyntax.Core.Program.SDD.Resolve.Engine (
      resolveDecls
    , resolveDecl
    , resolveExpr
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
import qualified Data.Either   as Either

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
import qualified SLIR.HelmSyntax.Core.Program.SDD.Inference.Utils.TypeSystem       as TS
import qualified SLIR.HelmSyntax.Core.Program.SDD.Solver.Data.Constraint           as Con
import qualified SLIR.HelmSyntax.Core.Program.SDD.Solver.Engine                    as Solver
import qualified SLIR.HelmSyntax.Core.Program.SDD.Subst.Expr                       as ExSub
import qualified SLIR.HelmSyntax.Core.Program.SDD.Subst.Types                      as TySub

-- ~ Inits
import qualified SLIR.HelmSyntax.Core.Program.SDD.Init.Decls as Init

-- ~ Misc. Helpers
import qualified SLIR.HelmSyntax.Core.Program.SDD.Resolve.Update.AltAbs as Update
-- *







-- *
-- | Resolve Infered Syntax
-- *



resolveDecls :: (Decl.Function -> Sys.Syntax Decl.Function)
             -> Env.ExprEnv
             -> [Decl.Function]
             -> Either Report.TypeError ([Decl.Function], Env.ExprEnv, [Con.Constraint])

resolveDecls f env1 decls1 =
    let
        (decls2, env2) = Init.initDecls decls1
        
        newEnv = Map.union env2 env1
        
    in
        resolveDecls' f (Map.union env2 env1) decls2


resolveDecls' :: (Decl.Function -> Sys.Syntax Decl.Function)
             -> Env.ExprEnv
             -> [Decl.Function]
             -> Either Report.TypeError ([Decl.Function], Env.ExprEnv, [Con.Constraint])
resolveDecls' f env []       = Right ([], env, [])


resolveDecls' f env1 (fn1:fns) =
    case resolveDecl f env1 fn1 of
        Left err -> Left err
        Right (fn2, env2, cs) ->
            finish env2 cs fn2 fns
    
    where
        
        finish :: Env.ExprEnv
               -> [Con.Constraint]
               -> Decl.Function
               -> [Decl.Function]
               -> Either Report.TypeError ([Decl.Function], Env.ExprEnv, [Con.Constraint])
        finish env cs fn fns =
            case resolveDecls' f env fns of
                Left err -> Left err
                Right (rest, env1, cs1) ->
                    Right (fn : rest, env1, cs ++ cs1)







resolveDecl :: (Decl.Function -> Sys.Syntax Decl.Function)
            -> Env.ExprEnv
            -> Decl.Function
            -> Either Report.TypeError (Decl.Function, Env.ExprEnv, [Con.Constraint])
resolveDecl f env0 fn =
    case Sys.runInfer env0 (f fn) of
        Left err -> Left err
        Right (fn1, ty, env1, cs) ->
            case (Solver.runTypeSolver cs, Solver.runExprSolver cs) of
                (Right tySubs, Right exSubs) ->
                    Right $ solution fn1 ty env1 tySubs exSubs cs
                
                (Left err, _) -> Left err
                (_, Left err) -> Left err



resolveExpr :: (E.Expr -> Sys.Syntax E.Expr)
            -> Env.ExprEnv
            -> E.Expr
            -> Either Report.TypeError (E.Expr, Env.ExprEnv, [Con.Constraint])
resolveExpr f env0 fn =
    case Sys.runInfer env0 (f fn) of
        Left err -> Left err
        Right (fn1, ty, env1, cs) ->
            case (Solver.runTypeSolver cs, Solver.runExprSolver cs) of
                (Right tySubs, Right exSubs) ->
                    Right $ exprSolution fn1 ty env1 tySubs exSubs cs
                
                (Left err, _) -> Left err
                (_, Left err) -> Left err





-- *
-- | Internal Helpers
-- *


solution :: Decl.Function
      -> T.Type
      -> Env.ExprEnv
      -> TySub.TySub
      -> ExSub.Subst
      -> [Con.Constraint]
      -> (Decl.Function, Env.ExprEnv, [Con.Constraint])

solution fn t env tySubs exSubs cs =
    let scheme = TS.closeOver $ TySub.apply tySubs t
        fn'    = ExSub.apply exSubs fn
        
        newFunction =
            Update.updateAltAbs (updateSig fn' scheme) t env tySubs exSubs
        
        
        newEnv = extend scheme newFunction env

    in
        (newFunction, newEnv, cs)



exprSolution :: E.Expr
             -> T.Type
             -> Env.ExprEnv
             -> TySub.TySub
             -> ExSub.Subst
             -> [Con.Constraint]
             -> (E.Expr, Env.ExprEnv, [Con.Constraint])

exprSolution expr t env tySubs exSubs cs =
    let scheme = TS.closeOver $ TySub.apply tySubs t
        expr'    = ExSub.apply exSubs expr
        -- newEnv = extend scheme newFunction env

    in
        (expr, env, cs)


-- |
-- Ensure that despite contrasting metadata,
-- the new functions will override the original entry.
--
extend :: T.Scheme
       -> Decl.Function
       -> Env.ExprEnv
       -> Env.ExprEnv
extend ty fn env =
    let name = CID.ident fn
    in
        Map.insert name (Env.Normal ty) env



updateSig :: Decl.Function -> T.Scheme -> Decl.Function
updateSig (Decl.FnDecl name args expr _ optMeta) scheme =
    let signature = Just $ Etc.Validated' scheme
    in
        Decl.FnDecl name args expr signature optMeta

updateSig (Decl.OpDecl sym args expr _ optMeta) scheme =
    let signature = Just $ Etc.Validated' scheme
    in
        Decl.OpDecl sym args expr signature optMeta





noMeta :: (Data a, Typeable a) => a -> a
noMeta =
    Uni.transformBi f
    where
        f :: Maybe Meta.Meta -> Maybe Meta.Meta
        f _ = Nothing
        


