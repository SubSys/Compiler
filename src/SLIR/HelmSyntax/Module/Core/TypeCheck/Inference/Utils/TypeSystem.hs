{-# LANGUAGE NoImplicitPrelude #-}
module SLIR.HelmSyntax.Module.Core.TypeCheck.Inference.Utils.TypeSystem (
    closeOver
  , instantiate
  , generalize
  , freshTSPair
  , freshScheme
  , freshType
  , normalize
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
import qualified SLIR.HelmSyntax.Module.Core.TypeCheck.Inference.Data.System          as Sys
import qualified SLIR.HelmSyntax.Module.Core.TypeCheck.Solver.Data.Constraint         as Con
import qualified SLIR.HelmSyntax.Module.Core.TypeCheck.Data.Report                    as Report
import qualified SLIR.HelmSyntax.Module.Core.TypeCheck.Inference.Data.Env             as Env
import qualified SLIR.HelmSyntax.Module.Core.TypeCheck.Subst.Types                    as TySub
import qualified SLIR.HelmSyntax.Module.Core.TypeCheck.Inference.Internal.FreshIdents as Fresh
-- *





-- | Canonicalize and return the polymorphic toplevel type.
--
closeOver :: T.Type -> T.Scheme
closeOver = normalize . generalize Map.empty


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

    return $ T.Var' ident



-- *
-- | Internal
-- *



normalize :: T.Scheme -> T.Scheme
normalize (T.Forall _ body) =
    T.Forall (map snd ord) (normtype body)

    where
        ord = List.zip (List.nub $ fv body) (map ID.Ident_ Fresh.letters)

        toLow = ID.Ident_

        fv x = [id' | (T.Var id' _) <- Uni.universe x]


        normtype = Uni.transformBi normtype'

        normtype' (T.Var id' _) =
            case Pre.lookup id' ord of
                Just x -> T.Var' x
                Nothing -> error "type variable not in signature"

        normtype' x = x





