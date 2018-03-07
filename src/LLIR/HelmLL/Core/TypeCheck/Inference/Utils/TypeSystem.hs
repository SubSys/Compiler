{-# LANGUAGE NoImplicitPrelude #-}
module LLIR.HelmLL.Core.TypeCheck.Inference.Utils.TypeSystem (
    closeOver
  , instantiate
  , generalize
  , freshTSPair
  , freshScheme
  , freshType
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
import qualified LLIR.HelmLL.Core.TypeCheck.Inference.Data.System          as Sys
import qualified LLIR.HelmLL.Core.TypeCheck.Solver.Data.Constraint         as Con
import qualified LLIR.HelmLL.Core.TypeCheck.Data.Report                    as Report
import qualified LLIR.HelmLL.Core.TypeCheck.Inference.Data.Env             as Env
import qualified LLIR.HelmLL.Core.TypeCheck.Subst.Types                    as TySub
import qualified LLIR.HelmLL.Core.TypeCheck.Inference.Internal.FreshIdents as Fresh
-- *





-- | Canonicalize and return the polymorphic toplevel type.
--
closeOver :: T.Type -> T.Scheme
closeOver = generalize Map.empty


instantiate ::  T.Scheme -> Sys.Infer T.Type
instantiate (T.Forall as t) = do
    as' <- M.mapM (const freshType) as
    -- *

    let s = TySub.Subst $ Map.fromList $ List.zip as as'

    -- *
    return $ TySub.apply s t


generalize :: Env.Types -> T.Type -> T.Scheme
generalize env t  =
    let
        xs = Set.toList $ TySub.fvs t `Set.difference` TySub.fvs env
        -- ys = map ID.toLow xs
    in
        T.Forall xs t





-- |
-- 'Fresh Type-Scheme Pair'
freshTSPair :: Sys.Infer (T.Type, T.Scheme)
freshTSPair = do
    t <- freshType

    return (t, T.Forall [] t)

freshScheme :: Sys.Infer T.Scheme
freshScheme = do
    t <- freshType

    return $ T.Forall [] t

freshType :: Sys.Infer T.Type
freshType = do
    ident <- Fresh.freshIdent

    return $ T.Var ident



-- *
-- | Internal
-- *







