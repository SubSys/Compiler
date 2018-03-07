{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns #-}
module LLIR.HelmLL.Core.TypeCheck.Inference.Engine (
    resolveDecls
  , resolveDecl
) where



-- *
import Core
import Core.Control.Flow ((|>), (<|))
import Core.List.Util    (flatten, singleton)
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
import qualified Data.Data                    as Data
import qualified Data.String                  as String

-- + Recursion Schemes & Related
import qualified Data.Functor.Foldable       as F
import qualified Data.Generics.Uniplate.Data as Uni

-- + Dev & Debugging
import qualified Text.Show.Prettyprint as PP




-- + HelmLL Module Interface
import qualified LLIR.HelmLL.Data.Interface as I

-- + HelmLL AST Utils
import qualified LLIR.HelmLL.AST.Utils.Generic.Scope       as Scope
import qualified LLIR.HelmLL.AST.Utils.Class.Ident         as ID
import qualified LLIR.HelmLL.AST.Utils.Auxiliary.Functions as Fn
import qualified LLIR.HelmLL.AST.Utils.Generic.SudoFFI     as SudoFFI

-- + HelmLL AST
-- ++ Base
import qualified LLIR.HelmLL.AST.Data.Base.Etc      as Etc
import qualified LLIR.HelmLL.AST.Data.Base.Ident    as ID
import qualified LLIR.HelmLL.AST.Data.Base.Types    as T
import qualified LLIR.HelmLL.AST.Data.Base.Literals   as V

-- ++ TermLevel
import qualified LLIR.HelmLL.AST.Data.TermLevel.Stmt     as S
import qualified LLIR.HelmLL.AST.Data.TermLevel.Patterns as P

-- ++ TopLevel
import qualified LLIR.HelmLL.AST.Data.TopLevel.Functions as Decl
import qualified LLIR.HelmLL.AST.Data.TopLevel.Unions    as Decl


-- + Local
import qualified LLIR.HelmLL.Core.TypeCheck.Inference.Data.System           as Sys
import qualified LLIR.HelmLL.Core.TypeCheck.Data.Report                     as Report
import qualified LLIR.HelmLL.Core.TypeCheck.Inference.Data.Env              as Env
import qualified LLIR.HelmLL.Core.TypeCheck.Inference.Utils.TypeSystem      as TS
import qualified LLIR.HelmLL.Core.TypeCheck.Inference.Syntax.Scope          as Scope
import qualified LLIR.HelmLL.Core.TypeCheck.Inference.Utils.General         as Util
import qualified LLIR.HelmLL.Core.TypeCheck.Solver.Data.Constraint          as Con
import qualified LLIR.HelmLL.Core.TypeCheck.Solver.Engine                   as Solver
import qualified LLIR.HelmLL.Core.TypeCheck.Subst.Types                     as TySub
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
        (updateBinders s newFunction, newEnv, cs)



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
updateSig (Decl.Function name args expr _) scheme =
    let signature = scheme
    in
        Decl.Function name args expr (Just signature)


updateBinders :: TySub.Subst -> Decl.Function -> Decl.Function
updateBinders subs = Uni.transformBi f
    where
        f :: Etc.Binder -> Etc.Binder
        f (Etc.Binder ident (isVar -> Just ty)) =
            let resType = TySub.apply subs ty
            in
                Etc.Binder ident (Just resType)
        
        f x = x



isVar :: Maybe T.Type -> Maybe T.Type
isVar (Just (x@T.Var{})) = Just x
isVar _ = Nothing


