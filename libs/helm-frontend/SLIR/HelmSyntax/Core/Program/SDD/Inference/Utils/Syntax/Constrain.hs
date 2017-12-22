{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
module SLIR.HelmSyntax.Core.Program.SDD.Inference.Utils.Syntax.Constrain (
    unify
  , app
  , binOpApp
  , unifySignature
  , unifyScheme
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
import qualified SLIR.HelmSyntax.Core.Program.SDD.Inference.Data.Env           as Env
import qualified SLIR.HelmSyntax.Core.Program.SDD.Solver.Data.Constraint       as Con
import qualified SLIR.HelmSyntax.Core.Program.SDD.Data.Report                  as Report
import qualified SLIR.HelmSyntax.Core.Program.SDD.Inference.Data.System        as Sys
import qualified SLIR.HelmSyntax.Core.Program.SDD.Inference.Utils.TypeSystem   as TS
import qualified SLIR.HelmSyntax.Core.Program.SDD.Inference.Utils.Syntax.Scope as Scope

import qualified SLIR.HelmSyntax.Core.Program.SDD.Inference.Utils.Overloaded as Overloaded
-- *





-- |
-- Simply generate a constraint on the two provided types.
--
unify :: T.Type -> T.Type -> Sys.Infer ()
unify t1 t2 = M.tell [Con.TypeCon t1 t2]


app :: T.Type -> T.Type -> Sys.Infer T.Type
app t1 t2 = do
    tv <- TS.freshType
    
    unify t1 (t2 `T.Arr'` tv)
    
    return tv



binOpApp :: ID.Sym -> T.Type -> T.Type -> Sys.Infer T.Type
binOpApp sym t1 t2 = do
    res <- Scope.isOverloaded sym
    case res of
        Nothing -> binOpAppDefault sym t1 t2
        Just sc -> 
            overloadedBinOp sc sym t1 t2







-- | Unify 'Type' & 'Function Signature'
-- If there exists a type signature, unify such with the provided type
--
unifySignature :: T.Type -> Maybe Etc.Signature -> Sys.Infer ()
unifySignature t Nothing =
    M.tell []

unifySignature t1 (Just (Etc.Unresolved t2 meta)) =
    unify t1 t2

unifySignature t1 (Just (Etc.Validated scheme meta)) = do
    t2 <- TS.instantiate scheme
    
    unify t1 t2
    -- M.tell []



unifyScheme :: T.Type -> Maybe T.Scheme -> Sys.Infer ()
unifyScheme t Nothing =
    M.tell []

unifyScheme t1 (Just scheme) = do
    t2  <- TS.instantiate (TS.normalize scheme)
    -- *
    
    
    -- *
    unify t1 t2







binOpAppDefault :: ID.Sym -> T.Type -> T.Type -> Sys.Infer T.Type
binOpAppDefault sym et1 et2 = do
    tv <- TS.freshType
    -- *

    -- *
    let t1 = et1 `T.Arr'` (et2 `T.Arr'` tv)
    t2 <- Scope.lookupEnv' sym
    -- *

    -- *
    unify t1 t2
    -- *

    return tv


-- Should I use overloaded or superposed?

-- I.e. overloaded [t1 : ts]

overloadedBinOp :: [(T.Scheme, E.Expr)] -> ID.Sym -> T.Type -> T.Type -> Sys.Infer T.Type
overloadedBinOp ss sym et1 et2 = do
    ts <- M.mapM TS.instantiate (map fst ss)
    -- *
    
    -- *
    tv <- TS.freshType
    
    superTv <- TS.freshType
    -- *

    -- *
    let t1 = et1 `T.Arr'` (et2 `T.Arr'` tv)
        t2 = T.Superposed superTv ts
    -- *
    
    -- *
    M.tell [Con.Overloaded (CID.ident sym) t1 (Map.fromList ss)]
    -- *

    -- *
    unify t1 t2
    -- *
    
    
    -- *
    return tv








