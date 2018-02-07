{-# LANGUAGE NoImplicitPrelude #-}
module SLIR.HelmSyntax.Module.Core.TypeCheck.Inference.Engine (
    resolveDecls
  , resolveDecl
) where



-- *
import Core
import Core.Control.Flow ((|>), (<|))
import Core.List.Util    (flatten, singleton)
import Data.Monoid ((<>))
import Prelude
    ( return
    , String
    , IO
    , show
    , error
    , (<$>)
    , (>>=)
    , (>>)
    , fromIntegral
    )

import qualified Prelude    as Pre
import qualified Core.Utils as Core

import qualified Control.Monad              as M
import qualified Control.Monad.State        as M
import qualified Control.Monad.Except       as M
import qualified Control.Monad.RWS          as M
import qualified Control.Monad.Identity     as M
import qualified Control.Monad.Reader       as M
import qualified Control.Monad.Writer       as M
import qualified Control.Monad.Trans        as M

import qualified Data.List                    as List
import qualified Data.Text                    as Text
import qualified Data.Text.IO                 as TIO
import qualified Data.Map                     as Map
import qualified Data.Set                     as Set
import qualified Data.Foldable                as Fold
import qualified Data.Monoid                  as Monoid
import qualified Data.Maybe                   as Maybe
import qualified Data.Either                  as Either
import qualified Data.Char                    as Char
import qualified Data.Word                    as Word
import qualified Data.STRef                   as ST
import qualified Data.Bits                    as Bit
import qualified Data.Fixed                   as Fixed
import qualified Data.Vector.Unboxed          as V
import qualified Data.Vector.Unboxed.Mutable  as MV
import qualified Data.Vector.Generic          as VG
import qualified Data.IORef                   as IORef
import qualified Data.ByteString              as BS
import qualified Data.Functor                 as Fun


-- + Recursion Schemes & Related
import qualified Data.Functor.Foldable       as F
import qualified Data.Generics.Uniplate.Data as Uni


-- + Dev & Debugging
import qualified Text.Show.Prettyprint as PP

-- + HelmSyntax Module Interface
import qualified SLIR.HelmSyntax.Module.Data.Interface as I

-- + HelmSyntax AST Utils
import qualified SLIR.HelmSyntax.AST.Utils.Scope           as Scope
import qualified SLIR.HelmSyntax.AST.Utils.Auxiliary.Ident as ID

-- + HelmSyntax AST
-- ++ Base
import qualified SLIR.HelmSyntax.AST.Data.Semantic.Base.Etc      as Etc
import qualified SLIR.HelmSyntax.AST.Data.Semantic.Base.Ident    as ID
import qualified SLIR.HelmSyntax.AST.Data.Semantic.Base.Types    as T
import qualified SLIR.HelmSyntax.AST.Data.Semantic.Base.Values   as V
import qualified SLIR.HelmSyntax.AST.Data.Semantic.Base.Metadata as Meta

-- ++ TermLevel
import qualified SLIR.HelmSyntax.AST.Data.Semantic.TermLevel.Expr     as E
import qualified SLIR.HelmSyntax.AST.Data.Semantic.TermLevel.Patterns as P

-- ++ TopLevel
import qualified SLIR.HelmSyntax.AST.Data.Semantic.TopLevel.Fixities  as Decl
import qualified SLIR.HelmSyntax.AST.Data.Semantic.TopLevel.Functions as Decl
import qualified SLIR.HelmSyntax.AST.Data.Semantic.TopLevel.Unions    as Decl

-- + Local
import qualified SLIR.HelmSyntax.Module.Core.TypeCheck.Inference.Data.System           as Sys
import qualified SLIR.HelmSyntax.Module.Core.TypeCheck.Data.Report                     as Report
import qualified SLIR.HelmSyntax.Module.Core.TypeCheck.Inference.Data.Env              as Env
import qualified SLIR.HelmSyntax.Module.Core.TypeCheck.Inference.Utils.TypeSystem      as TS
import qualified SLIR.HelmSyntax.Module.Core.TypeCheck.Inference.Syntax.Scope          as Scope
import qualified SLIR.HelmSyntax.Module.Core.TypeCheck.Inference.Utils.General         as Util
import qualified SLIR.HelmSyntax.Module.Core.TypeCheck.Solver.Data.Constraint          as Con
import qualified SLIR.HelmSyntax.Module.Core.TypeCheck.Solver.Engine                   as Solver
import qualified SLIR.HelmSyntax.Module.Core.TypeCheck.Subst.Types                     as TySub
import qualified SLIR.HelmSyntax.Module.Core.TypeCheck.Inference.Engine.Update.Binders as Update
-- *





{-# ANN module ("HLint: ignore" :: String) #-}




-- *
-- | Resolve Infered Syntax
-- *


resolveDecls :: (Decl.Function -> Sys.Syntax Decl.Function)
             -> Env.Env
             -> [Decl.Function]
             -> Either Report.TypeError ([Decl.Function], Env.Types, [Con.Constraint])
resolveDecls f env []       = Right ([], fst env, [])
resolveDecls f env (fn:fns) =
    case resolveDecl f env fn of
        Left err -> Left err
        Right (fn', env', cs) ->
            finish env' cs fn' fns
    
    where
        overloads = snd env
        
        finish :: Env.Types
               -> [Con.Constraint]
               -> Decl.Function
               -> [Decl.Function]
               -> Either Report.TypeError ([Decl.Function], Env.Types, [Con.Constraint])
        finish env cs fn fns =
            case resolveDecls f (env,  overloads) fns of
                Left err -> Left err
                Right (rest, env', cs') ->
                    Right (fn : rest, env', cs ++ cs')



resolveDecl :: (Decl.Function -> Sys.Syntax Decl.Function)
            -> Env.Env
            -> Decl.Function
            -> Either Report.TypeError (Decl.Function, Env.Types, [Con.Constraint])
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
      -> Env.Types
      -> TySub.Subst
      -> [Con.Constraint]
      -> (Decl.Function, Env.Types, [Con.Constraint])

solution fn t env s cs =
    let scheme = TS.generalize Map.empty $ TySub.apply s t
        newFunction = updateSig fn scheme
        newEnv = extend scheme newFunction env
        
    in
        (Update.updateBinders s newFunction, newEnv, cs)



-- |
-- Ensure that despite contrasting metadata,
-- the new functions will override the original entry.
--
extend :: T.Scheme
       -> Decl.Function
       -> Env.Types
       -> Env.Types
extend ty fn env =
    let name = ID.get fn
    in
        Map.insert name ty env



updateSig :: Decl.Function -> T.Scheme -> Decl.Function
updateSig (Decl.Function name args expr _ optMeta) scheme =
    let signature = Decl.Validated scheme Meta.Empty
    in
        Decl.Function name args expr signature optMeta



