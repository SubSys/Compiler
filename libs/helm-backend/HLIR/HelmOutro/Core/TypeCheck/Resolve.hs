{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns #-}
module HLIR.HelmOutro.Core.TypeCheck.Resolve (
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

import qualified Data.Generics.Uniplate.Data as Uni
import qualified Text.Show.Prettyprint as PP


--- Local Deps
-- ~ HelmOutro IR
import qualified HLIR.HelmOutro.Data.Payload as Payload

-- ~ HelmOutro AST
-- ~~ Base
import qualified HLIR.HelmOutro.AST.Base.Ident  as ID
import qualified HLIR.HelmOutro.AST.Base.Types  as T
import qualified HLIR.HelmOutro.AST.Base.Values as V
import qualified HLIR.HelmOutro.AST.Base.Etc    as Etc
-- ~~ TermLevel
import qualified HLIR.HelmOutro.AST.TermLevel.Expressions as E
import qualified HLIR.HelmOutro.AST.TermLevel.Patterns    as P
-- ~~ TopLevel
import qualified HLIR.HelmOutro.AST.TopLevel.Functions as Decl
import qualified HLIR.HelmOutro.AST.TopLevel.Unions    as Decl


--- Local
import qualified HLIR.HelmOutro.Core.TypeCheck.Data.Env                    as Env
import qualified HLIR.HelmOutro.Core.TypeCheck.Data.Report                 as Report
import qualified HLIR.HelmOutro.Core.TypeCheck.Data.Subst                  as Sub
import qualified HLIR.HelmOutro.Core.TypeCheck.Data.System                 as Sys
import qualified HLIR.HelmOutro.Core.TypeCheck.Data.TypeSystem             as TS
import qualified HLIR.HelmOutro.Core.TypeCheck.Data.Canonical.Ident        as CID
import qualified HLIR.HelmOutro.Core.TypeCheck.Data.System.Scope           as Scope
import qualified HLIR.HelmOutro.Core.TypeCheck.Data.Unification.Solver     as Solver
import qualified HLIR.HelmOutro.Core.TypeCheck.Data.Unification.Constraint as Con

import qualified HLIR.HelmOutro.Core.TypeCheck.Sudo.FFI.Helpers as SudoFFI
import qualified HLIR.HelmOutro.Core.TypeCheck.Resolve.UpdateArgs as UpdateArgs
-- *



{-# ANN module ("HLint: ignore" :: String) #-}




-- *
-- | Resolve Infered Syntax
-- *


resolveDecls :: (Decl.Function -> Sys.Syntax Decl.Function)
             -> Env.Env
             -> [Decl.Function]
             -> Either Report.TypeError ([Decl.Function], Env.Env, [Con.Constraint])
resolveDecls f env []       = Right ([], env, [])
resolveDecls f env (fn:fns) =
    case resolveDecl f env fn of
        Left err -> Left err
        Right (fn', env', cs) ->
            finish env' cs fn' fns
    
    where
        finish :: Env.Env
               -> [Con.Constraint]
               -> Decl.Function
               -> [Decl.Function]
               -> Either Report.TypeError ([Decl.Function], Env.Env, [Con.Constraint])
        finish env cs fn fns =
            case resolveDecls f env fns of
                Left err -> Left err
                Right (rest, env', cs') ->
                
                    -- TODO: Bug? I.e.  change `cs ++ cs'` to `cs'`?
                    
                    Right (fn : rest, Env.merge env' env, cs ++ cs')



resolveDecl :: (Decl.Function -> Sys.Syntax Decl.Function)
            -> Env.Env
            -> Decl.Function
            -> Either Report.TypeError (Decl.Function, Env.Env, [Con.Constraint])
resolveDecl f env0 fn =
    case Sys.runInfer env0 (f fn) of
        Left err -> Left err
        Right (fn', ty, env1, cs) ->
            case Solver.runSolve cs of
                Left err -> Left err
                Right subst ->
                    Right $ solution fn' ty (Env.merge env1 env0) subst cs





-- *
-- | Internal Helpers
-- *



solution :: Decl.Function
      -> T.Type
      -> Env.Env
      -> Sub.Subst
      -> [Con.Constraint]
      -> (Decl.Function, Env.Env, [Con.Constraint])

solution fn t env s cs =
    let scheme = TS.generalize Env.empty $ Sub.apply s t
    
        newEnv = extend scheme newFunction env
        
        newFunction = UpdateArgs.updateDecl s $ updateSig fn scheme
        
    in
        (newFunction, newEnv, cs)


extend :: T.Scheme
       -> Decl.Function
       -> Env.Env
       -> Env.Env
extend ty fn env =
    let name = CID.ident fn
    in
        Env.extend env (name, ty)



updateSig :: Decl.Function -> T.Scheme -> Decl.Function
updateSig (Decl.Function name args expr _) scheme =
    Decl.Function name args expr (Just scheme)










