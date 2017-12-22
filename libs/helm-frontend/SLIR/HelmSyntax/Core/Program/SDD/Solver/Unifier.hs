{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
module SLIR.HelmSyntax.Core.Program.SDD.Solver.Unifier (
    unifyType
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

-- ~~ Auxiliary Nodes
import qualified SLIR.HelmSyntax.AST.Auxiliary.Canonical.Ident as CID


--- Local
import qualified SLIR.HelmSyntax.Core.Program.SDD.Inference.Data.Env               as Env
import qualified SLIR.HelmSyntax.Core.Program.SDD.Data.Report                      as Report
import qualified SLIR.HelmSyntax.Core.Program.SDD.Inference.Utils.TypeSystem       as TS
import qualified SLIR.HelmSyntax.Core.Program.SDD.Solver.Data.Constraint           as Con
import qualified SLIR.HelmSyntax.Core.Program.SDD.Solver.Data.System               as Sys
import qualified SLIR.HelmSyntax.Core.Program.SDD.Subst.Types          as TySub
import qualified SLIR.HelmSyntax.Core.Program.SDD.Subst.Expr           as ExSub
-- *




-- *
-- | Unification
-- *


unifyType :: T.Type -> T.Type -> Sys.Solve TySub.TySub
unifyType t1 t2 | t1 == t2 = return TySub.emptySubst


unifyType (T.Var v _) t = v `bind` t
unifyType t (T.Var v _) = v `bind` t


unifyType (T.List t1 _) (T.List t2 _) = unifyType t1 t2

unifyType (T.Parens t1 _) t2 = unifyType t1 t2
unifyType t1 (T.Parens t2 _) = unifyType t1 t2

unifyType (T.Arr t1 t2 _) (T.Arr t3 t4 _) =
    unifyTypeMany [t1, t2] [t3, t4]

unifyType t1@(T.Union name1 args1 _) t2@(T.Union name2 args2 _)
    | name1 == name2 =
        unifyTypeMany args1 args2
    
    
    | otherwise =
        M.throwError $ Report.UnificationFail t1 t2




unifyType t1 (T.Superposed con ts) = unifyOverloaded con t1 ts
unifyType (T.Superposed con ts) t1 = unifyOverloaded con t1 ts


unifyType t1 t2 = M.throwError $ Report.UnificationFail t1 t2



bind ::  ID.Low -> T.Type -> Sys.Solve TySub.TySub
bind a t | t == T.Var' a     = return TySub.emptySubst
         | occursCheck a t   = M.throwError $ Report.InfiniteType a' t
         | otherwise         = return (TySub.TySub $ Map.singleton a' t)
    where
        a' = CID.ident a






-- *
-- | Internal Utils
-- *


occursCheck :: ID.Low -> T.Type -> Bool
occursCheck a t = CID.ident a `Set.member` TySub.fvs t





unifyTypeMany :: [T.Type] -> [T.Type] -> Sys.Solve TySub.TySub
unifyTypeMany [] [] = return TySub.emptySubst
unifyTypeMany (t1 : ts1) (t2 : ts2) =
    do
        su1 <- unifyType t1 t2
        su2 <- unifyTypeMany (TySub.apply su1 ts1) (TySub.apply su1 ts2)
        return (su2 `TySub.compose` su1)

unifyTypeMany t1 t2 = M.throwError $ Report.UnificationMismatch t1 t2









unifyOverloaded :: T.Type -> T.Type -> [T.Type] -> Sys.Solve TySub.TySub
unifyOverloaded con@(T.Var name _) t1 ts
    | isEmptySubst valids =
        
        return $ TySub.TySub $ Map.singleton (CID.ident name) t1

    | not (null valids) =
        toSuperposed con valids

    | otherwise =
        M.throwError (Report.OverloadedTypeFail t1 ts)


    where

        result = map (\x -> fst $ M.runState (M.runExceptT (unifyTrial x)) Sys.initCounter) ts

        valids   = Either.rights result
        invalids = Either.lefts result


        unifyTrial = unifyType t1



unifyOverloaded con t1 ts = do
    name <- freshIdent
    
    return $ TySub.TySub $ Map.singleton name con


toSuperposed :: T.Type -> [TySub.TySub] -> Sys.Solve TySub.TySub
toSuperposed con subs =
    return $ TySub.TySub $ Map.unionsWith toSuper $ map extract subs

    where
        prefix = Text.pack "!"
        label x = prefix `Text.append` Text.pack (show x)

        extract :: TySub.TySub -> Map.Map CID.Ident T.Type
        extract (TySub.TySub s) = s

        toSuper :: T.Type -> T.Type -> T.Type
        toSuper t1 t2 = T.Superposed con [t1, t2]





isEmptySubst :: [TySub.TySub] -> Bool
isEmptySubst [TySub.TySub x]
    | Map.empty == x = True


isEmptySubst _ = False


freshIdent :: Sys.Solve CID.Ident
freshIdent = do
    (Sys.Counter i) <- M.get
    M.put (Sys.Counter $ i + 1)
    
    return $ CID.Ident (label i) Nothing Nothing

    where
        prefix = Text.pack "!"
        label x = prefix `Text.append` Text.pack (show x)












