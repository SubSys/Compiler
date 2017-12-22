{-# LANGUAGE NoImplicitPrelude #-}
module SLIR.HelmSyntax.Core.Program.SDD.PostWork.Lift (
    processDecls
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

import qualified Data.List           as List
import qualified Data.Text           as Text
import qualified Data.Map            as Map
import qualified Data.Set            as Set
import qualified Data.Foldable       as Fold
import qualified Data.Monoid         as Monoid
import qualified Data.Either         as Either
import qualified Data.Maybe          as Maybe
import qualified Data.Hashable       as Hash
import qualified Data.HashMap.Strict as HMap


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

-- ~~ Auxiliary AST - Nodes & Utils
import qualified SLIR.HelmSyntax.AST.Auxiliary.Canonical.Ident            as CID
import qualified SLIR.HelmSyntax.AST.Toolbox.SudoFFI                      as SudoFFI
import qualified SLIR.HelmSyntax.AST.Toolbox.TopLevel.Functions.Recursive as Rec


-- ~~ AST Instances
import SLIR.HelmSyntax.AST.Instances.Hashable ()

--- Local
import qualified SLIR.HelmSyntax.Core.Program.SDD.Inference.Utils.TypeSystem       as TS
-- *


-- | TODO: Hash contents then compare such rather than compare the expression nodes themselves.
--



-- *
-- | Data Types
-- *

newtype Counter = Counter Int

type Env = HMap.HashMap Int CID.Ident

type Lift a = M.WriterT [Decl.Function]
    (M.State
        (Counter, Env))
    a




-- *
-- | Lifter
-- *


processDecls :: [Decl.Function] -> [Decl.Function]
processDecls decls =
    let 
        ((decls', newDecls), _) = M.runState (M.runWriterT (Uni.transformBiM liftExpr decls)) initState
    in
        newDecls ++ decls'
    
    where
        initState = (Counter 0, HMap.empty)


-- censor :: ([Decl.Function] -> [Decl.Function]) -> Lift a -> Lift a
-- censor xs =

-- censor' :: Decl.Function -> Lift (Maybe Decl.Function)
-- censor' fn = do
--     (_, env) <- M.ask
-- 
--     case Map.lookup (CID.ident fn) env of
--         Just ident ->


liftExpr :: E.Expr -> Lift E.Expr


liftExpr (E.AltAbs args expr scheme) =
    instantiate args expr scheme





liftExpr e = return e



-- *
-- | Misc. Utils/Helpers
-- *


incCounter :: Lift Int
incCounter = do
    (Counter i, x) <- M.get
    M.put (Counter $ i + 1, x)
    return i

updateEnv :: ([ID.Low], E.Expr, Maybe T.Scheme) -> CID.Ident -> Lift ()
updateEnv key val = do
    (c, m) <- M.get
    
    M.put (c, HMap.insert (Hash.hash key) val m)


lookupEnv :: ([ID.Low], E.Expr, Maybe T.Scheme) -> Env -> Maybe CID.Ident
lookupEnv key =
    HMap.lookup (Hash.hash key)


instantiate :: [ID.Low] -> E.Expr -> Maybe T.Scheme -> Lift E.Expr
instantiate args expr sig@(Just scheme) = do
    (_, env) <- M.get



    case lookupEnv (args, expr, sig) env of
        Just ident ->
            return $ E.Var' (CID.toLow ident)

        Nothing -> do
            i <- incCounter

            let
                name = toName i
                decl = Decl.FnDecl name args expr (Just (Etc.Validated scheme Nothing)) Nothing

            updateEnv (args, expr, sig) (CID.ident name)
            M.tell [decl]

            return $ E.Var' name

-- instantiate args expr sig@(Just scheme) = do
--     i <- incCounter
-- 
--     let
--         name = toName i
--         decl = Decl.FnDecl name args expr (Just (Etc.Validated scheme Nothing)) Nothing
-- 
--     updateEnv (args, expr, sig) (CID.ident name)
--     M.tell [decl]
-- 
--     return $ E.Var' name
    
    where
        prefix = Text.pack "!"
        toTxt = Text.pack . show
            
        toName x =
            ID.Low' (prefix `Text.append` toTxt x) Nothing





