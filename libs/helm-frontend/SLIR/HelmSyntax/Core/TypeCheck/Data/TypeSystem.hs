{-# LANGUAGE NoImplicitPrelude #-}
module SLIR.HelmSyntax.Core.TypeCheck.Data.TypeSystem (
      closeOver
    , instantiate
    , generalize
    , freshTSPair
    , freshScheme
    , freshType
    , freshIdent
) where


-- *
import Core
import Core.Control.Flow ((|>), (<|))
import Prelude (return, String, IO, show, error, (<$>), (!!), lookup, (>>=), (>>))

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
import qualified SLIR.HelmSyntax.Core.TypeCheck.Data.Unification.Constraint as Con
import qualified SLIR.HelmSyntax.Core.TypeCheck.Data.Report                 as Report
import qualified SLIR.HelmSyntax.Core.TypeCheck.Data.Env                    as Env
import qualified SLIR.HelmSyntax.AST.Auxiliary.Canonical.Ident        as CID
import qualified SLIR.HelmSyntax.Core.TypeCheck.Data.System                 as Sys
import qualified SLIR.HelmSyntax.Core.TypeCheck.Data.Subst                  as Sub
-- *




-- | Canonicalize and return the polymorphic toplevel type.
--
closeOver :: T.Type -> T.Scheme
closeOver = normalize . generalize Env.empty


instantiate ::  T.Scheme -> Sys.State T.Type
instantiate (T.Forall as t) = do
    as' <- M.mapM (const freshType) as
    -- *

    let s = Sub.Subst $ Map.fromList $ List.zip (CID.idents as) as'

    -- *
    return $ Sub.apply s t


generalize :: Env.Env -> T.Type -> T.Scheme
generalize env t  =
    let xs = Set.toList $ Sub.ftv t `Set.difference` Sub.ftv env
        ys = map CID.toLow xs
    in
        T.Forall ys t





-- |
-- 'Fresh Type-Scheme Pair'
freshTSPair :: Sys.State (T.Type, T.Scheme)
freshTSPair = do
    t <- freshType

    return (t, T.Forall [] t)

freshScheme :: Sys.State T.Scheme
freshScheme = do
    t <- freshType

    return $ T.Forall [] t

freshType :: Sys.State T.Type
freshType = do
    ident <- freshIdent

    return $ T.Var' ident



-- *
-- | Internal
-- *


freshIdent :: Sys.State ID.Low
freshIdent = do
    idx <- Sys.incCounter

    return $ pack (letters !! idx)

    where
        pack :: Text -> ID.Low
        pack x = ID.Low x Nothing Nothing


letters :: [Text]
letters =
    Text.pack <$> ([1..] >>= flip M.replicateM ['a'..'z'])



normalize :: T.Scheme -> T.Scheme
normalize (T.Forall _ body) =
    T.Forall (map snd ord) (normtype body)

    where
        ord = List.zip (List.nub $ fv body) (map (CID.toLow . CID.Ident') letters)

        toLow x = ID.Low x Nothing

        fv x = [id' | (T.Var id' _) <- Uni.universe x]


        normtype = Uni.transformBi normtype'

        normtype' (T.Var id' _) =
            case lookup id' ord of
                Just x -> T.Var' x
                Nothing -> error "type variable not in signature"

        normtype' x = x




