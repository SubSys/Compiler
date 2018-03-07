{-# LANGUAGE NoImplicitPrelude #-}
module LLIR.HelmLL.Core.TypeCheck.Inference.Utils.General (
    inferList
  , inferMaybe
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

-- + Local Prelude
import LLIR.HelmLL.Core.TypeCheck.Inference.Syntax.Base (enter)

-- + Local
import qualified LLIR.HelmLL.Core.TypeCheck.Inference.Data.System  as Sys
import qualified LLIR.HelmLL.Core.TypeCheck.Solver.Data.Constraint as Con
import qualified LLIR.HelmLL.Core.TypeCheck.Data.Report            as Report
import qualified LLIR.HelmLL.Core.TypeCheck.Inference.Data.Env     as Env
import qualified LLIR.HelmLL.Core.TypeCheck.Subst.Types            as TySub
import qualified LLIR.HelmLL.Core.TypeCheck.Inference.Syntax.Scope as Scope
-- *



inferList :: (a -> Sys.Syntax a) -> [a] -> Sys.Infer ([a], [T.Type], Env.Types)
inferList f []     = do
    env <- fst <$> M.ask
    
    return ([], [], env)


inferList f (x:xs) = do
    (x', t, e) <- f x
    (xs', ts, env') <- Scope.withLocalEnv e (inferList f xs)
    
    return (x' : xs', t : ts, env')


inferMaybe :: (a -> Sys.Syntax a) -> T.Type -> Maybe a -> Sys.Syntax (Maybe a)
inferMaybe f defaultType Nothing = enter Nothing defaultType
inferMaybe f defaultType (Just x) = do
    (x', t, e) <- f x

    return (Just x', t, e)


