{-# LANGUAGE NoImplicitPrelude #-}
module SLIR.HelmSyntax.Core.Module.TypeCheck.Data.System.Constraints (
      unify
    , app
    , binOpApp
    , unifySignature
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
import qualified SLIR.HelmSyntax.Core.Module.TypeCheck.Data.Unification.Constraint as Con
import qualified SLIR.HelmSyntax.Core.Module.TypeCheck.Data.Interface.TypesEnv     as TI
import qualified SLIR.HelmSyntax.Core.Module.TypeCheck.Data.Report                 as Report
import qualified SLIR.HelmSyntax.Core.Module.TypeCheck.Data.System                 as Sys
import qualified SLIR.HelmSyntax.Core.Module.TypeCheck.Data.TypeSystem             as TS
import qualified SLIR.HelmSyntax.Core.Module.TypeCheck.Data.System.Scope           as Scope
import qualified SLIR.HelmSyntax.AST.Auxiliary.Canonical.Ident              as CID
-- *


-- |
-- Simply generate a constraint on the two provided types.
--
unify :: T.Type -> T.Type -> Sys.Constrain
unify t1 t2 = M.tell [(t1, t2)]


app :: T.Type -> T.Type -> Sys.Constrain' T.Type
app t1 t2 = do
    tv <- TS.freshType
    
    unify t1 (t2 `T.Arr'` tv)
    
    return tv



binOpApp :: ID.Sym -> T.Type -> T.Type -> Sys.Constrain' T.Type
binOpApp sym t1 t2 = do
    res <- Scope.isOverloaded sym
    case res of
        Nothing -> binOpApp' sym t1 t2
        Just ts -> 
            overloadedBinOp ts sym t1 t2




-- | Unify 'Type' & 'Function Signature'
-- If there exists a type signature, unify such with the provided type
--
unifySignature :: T.Type -> Maybe Etc.Signature -> Sys.Constrain
unifySignature t Nothing =
    M.tell []

unifySignature t (Just (Etc.Unresolved t' meta)) =
    unify t t'





-- *
-- | Misc. Internal Helpers
-- *



-- | Default Constraints (not overloaded).
--


binOpApp' :: ID.Sym -> T.Type -> T.Type -> Sys.Constrain' T.Type
binOpApp' sym et1 et2 = do
    tv <- TS.freshType
    -- *

    -- *
    let t1 = et1 `T.Arr'` (et2 `T.Arr'` tv)
    t2 <- Scope.lookupSym sym
    -- *

    -- *
    unify t1 t2
    -- *

    return tv


-- Should I use overloaded or superposed?

-- I.e. overloaded [t1 : ts]

overloadedBinOp :: [T.Type] -> ID.Sym -> T.Type -> T.Type -> Sys.Constrain' T.Type
overloadedBinOp ts sym et1 et2 = do
    tv <- TS.freshType
    
    superTv <- TS.freshType
    -- *

    -- *
    let t1 = et1 `T.Arr'` (et2 `T.Arr'` tv)
        t2 = T.Superposed superTv ts
    -- *

    -- *
    unify t1 t2
    -- *
    
    
    -- *
    return tv



