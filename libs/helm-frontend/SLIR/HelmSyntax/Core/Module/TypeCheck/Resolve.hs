{-# LANGUAGE NoImplicitPrelude #-}
module SLIR.HelmSyntax.Core.Module.TypeCheck.Resolve (
      resolveDecls
    , resolveDecl
) where




-- *
import Core
import Core.Control.Flow ((|>), (<|))
import Prelude (return, String, IO, show, error, (<$>))

import Data.List.Index  (imap)

import qualified Control.Monad.State        as M
import qualified Control.Monad.Except       as M
import qualified Control.Monad.RWS          as M
import qualified Control.Monad.Identity     as M
import qualified Control.Monad.Reader       as M

import qualified Data.List     as List
import qualified Data.Text     as Text
import qualified Data.Map      as Map
import qualified Data.Set      as Set
import qualified Data.Foldable as Fold
import qualified Data.Monoid   as Monoid
import qualified Data.String   as String

import qualified Data.Generics.Uniplate.Data as Uni
import qualified Text.Show.Prettyprint as PP


--- Local Deps
-- ~ HelmSyntax AST
-- ~~ Base
import qualified SLIR.HelmSyntax.AST.Data.Base.Etc    as Etc
import qualified SLIR.HelmSyntax.AST.Data.Base.Ident  as ID
import qualified SLIR.HelmSyntax.AST.Data.Base.Types  as T
import qualified SLIR.HelmSyntax.AST.Data.Base.Values as V

-- ~~ TermLevel
import qualified SLIR.HelmSyntax.AST.Data.TermLevel.Expressions as E
import qualified SLIR.HelmSyntax.AST.Data.TermLevel.Patterns    as P

-- ~~ TopLevel
import qualified SLIR.HelmSyntax.AST.Data.TopLevel.Fixities  as Decl
import qualified SLIR.HelmSyntax.AST.Data.TopLevel.Functions as Decl
import qualified SLIR.HelmSyntax.AST.Data.TopLevel.Unions    as Decl

--- Local
import qualified SLIR.HelmSyntax.Core.Module.TypeCheck.Data.Interface.TypesEnv     as TI
import qualified SLIR.HelmSyntax.Core.Module.TypeCheck.Data.Report                 as Report
import qualified SLIR.HelmSyntax.Core.Module.TypeCheck.Data.Subst                  as Sub
import qualified SLIR.HelmSyntax.Core.Module.TypeCheck.Data.System                 as Sys
import qualified SLIR.HelmSyntax.Core.Module.TypeCheck.Data.TypeSystem             as TS
import qualified SLIR.HelmSyntax.AST.Auxiliary.Canonical.Ident              as CID
import qualified SLIR.HelmSyntax.Core.Module.TypeCheck.Data.System.Scope           as Scope
import qualified SLIR.HelmSyntax.Core.Module.TypeCheck.Data.Unification.Solver     as Solver
import qualified SLIR.HelmSyntax.Core.Module.TypeCheck.Data.Unification.Constraint as Con
-- *



{-# ANN module ("HLint: ignore" :: String) #-}




-- *
-- | Resolve Infered Syntax
-- *


resolveDecls :: (Decl.Function -> Sys.Syntax Decl.Function)
             -> (TI.Env, Sys.Overloaded)
             -> [Decl.Function]
             -> Either Report.TypeError ([Decl.Function], TI.Env, [Con.Constraint])
resolveDecls f env []       = Right ([], fst env, [])
resolveDecls f env (fn:fns) =
    case resolveDecl f env fn of
        Left err -> Left err
        Right (fn', env', cs) ->
            finish env' cs fn' fns
    
    where
        overloads = snd env
        
        finish :: TI.Env
               -> [Con.Constraint]
               -> Decl.Function
               -> [Decl.Function]
               -> Either Report.TypeError ([Decl.Function], TI.Env, [Con.Constraint])
        finish env cs fn fns =
            case resolveDecls f (env,  overloads) fns of
                Left err -> Left err
                Right (rest, env', cs') ->
                    Right (fn : rest, env', cs ++ cs')



resolveDecl :: (Decl.Function -> Sys.Syntax Decl.Function)
            -> (TI.Env, Sys.Overloaded)
            -> Decl.Function
            -> Either Report.TypeError (Decl.Function, TI.Env, [Con.Constraint])
resolveDecl f env0 fn =
    case Sys.runInfer env0 (f fn) of
        Left err -> Left err
        Right (fn', ty, env1, cs) ->
            case Solver.runSolve cs of
                Left err -> Left err
                Right subst ->
                    Right $ solution fn' ty env1 subst cs






-- *
-- | Internal Helpers
-- *


solution :: Decl.Function
      -> T.Type
      -> TI.Env
      -> Sub.Subst
      -> [Con.Constraint]
      -> (Decl.Function, TI.Env, [Con.Constraint])

solution fn t env s cs =
    let scheme = TS.closeOver $ Sub.apply s t
        newFunction = updateSig fn scheme
        newEnv = extend scheme newFunction env
        
    in
        (newFunction, newEnv, cs)



-- |
-- Ensure that despite contrasting metadata,
-- the new functions will override the original entry.
--
extend :: T.Scheme
       -> Decl.Function
       -> TI.Env
       -> TI.Env
extend ty fn env =
    let name = CID.ident fn
    in
        TI.extend env (name, ty)



updateSig :: Decl.Function -> T.Scheme -> Decl.Function
updateSig (Decl.FnDecl name args expr _ optMeta) scheme =
    let signature = Just $ Etc.Validated' scheme
    in
        Decl.FnDecl name args expr signature optMeta

updateSig (Decl.OpDecl sym args expr _ optMeta) scheme =
    let signature = Just $ Etc.Validated' scheme
    in
        Decl.OpDecl sym args expr signature optMeta







