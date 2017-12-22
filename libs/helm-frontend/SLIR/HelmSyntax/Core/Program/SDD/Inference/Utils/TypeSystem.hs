{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
module SLIR.HelmSyntax.Core.Program.SDD.Inference.Utils.TypeSystem (
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
import Core.List.Util    (flatten)
import Data.Data (Data, Typeable)

import Prelude (return, String, IO, show, error, (<$>), (>>), (>>=), (>>), lookup)

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

-- ~~ Auxiliary Nodes
import qualified SLIR.HelmSyntax.AST.Auxiliary.Canonical.Ident as CID


--- Local Internal-Utils
import qualified SLIR.HelmSyntax.Core.Program.SDD.Inference.Utils.Internal.FreshIdents as Fresh

--- Local
import qualified SLIR.HelmSyntax.Core.Program.SDD.Inference.Data.Env         as Env
import qualified SLIR.HelmSyntax.Core.Program.SDD.Solver.Data.Constraint     as Con
import qualified SLIR.HelmSyntax.Core.Program.SDD.Data.Report                as Report
import qualified SLIR.HelmSyntax.Core.Program.SDD.Inference.Data.System      as Sys
import qualified SLIR.HelmSyntax.Core.Program.SDD.Subst.Types                as TySub
-- *



-- | Canonicalize and return the polymorphic toplevel type.
--
closeOver :: T.Type -> T.Scheme
closeOver = normalize . generalize Map.empty


instantiate ::  T.Scheme -> Sys.Infer T.Type
instantiate (T.Forall as t) = do
    as' <- M.mapM (const freshType) as
    -- *

    let s = TySub.TySub $ Map.fromList $ List.zip (CID.idents as) as'

    -- *
    return $ TySub.apply s t


generalize :: Env.ExprEnv -> T.Type -> T.Scheme
generalize env t  =
    let xs = Set.toList $ TySub.fvs t `Set.difference` TySub.fvs env
        ys = map CID.toLow xs
    in
        T.Forall ys t





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
        ord = List.zip (List.nub $ fv body) (map (CID.toLow . CID.Ident') Fresh.letters)

        toLow x = ID.Low x Nothing

        fv x = [id' | (T.Var id' _) <- Uni.universe x]


        normtype = Uni.transformBi normtype'

        normtype' (T.Var id' _) =
            case lookup id' ord of
                Just x -> T.Var' x
                Nothing -> error "type variable not in signature"

        normtype' x = x





