{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns #-}
module SLIR.HelmSyntax.Program.Core.Lift.Syntax (
    liftDecls
) where


-- *
import Core
import Core.Control.Flow ((|>), (<|))
import Core.List.Util    (flatten, singleton)
import Data.Monoid ((<>))
import Prelude
    (return
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
import qualified Data.String                  as String

-- + Recursion Schemes & Related
import qualified Data.Functor.Foldable       as F
import qualified Data.Generics.Uniplate.Data as Uni

-- + OS APIS & Related
import qualified System.IO as SIO

-- + Dev & Debugging
import qualified Text.Show.Prettyprint as PP




-- + HelmSyntax Module Interface
import qualified SLIR.HelmSyntax.Program.Data.Interface as I

-- + HelmSyntax AST Renderer
import qualified SLIR.HelmSyntax.AST.Render.Syntax.Driver as Syntax

-- + HelmSyntax AST Utils
import qualified SLIR.HelmSyntax.AST.Utils.Scope                         as Scope
import qualified SLIR.HelmSyntax.AST.Utils.Auxiliary.Ident               as ID
import qualified SLIR.HelmSyntax.AST.Utils.Auxiliary.Functions.SudoFFI   as SudoFFI
import qualified SLIR.HelmSyntax.AST.Utils.Auxiliary.Functions.Recursive as Rec
import qualified SLIR.HelmSyntax.AST.Utils.Auxiliary.Type                as T
import qualified SLIR.HelmSyntax.AST.Utils.Auxiliary.Binders             as Binder
import qualified SLIR.HelmSyntax.AST.Utils.Auxiliary.Fresh               as Fresh
import qualified SLIR.HelmSyntax.AST.Utils.Auxiliary.Expr                as Expr

-- + HelmSyntax AST
-- ++ Base
import qualified SLIR.HelmSyntax.AST.Data.Semantic.Base.Etc      as Etc
import qualified SLIR.HelmSyntax.AST.Data.Semantic.Base.Ident    as ID
import qualified SLIR.HelmSyntax.AST.Data.Semantic.Base.Types    as T
import qualified SLIR.HelmSyntax.AST.Data.Semantic.Base.Values   as V
import qualified SLIR.HelmSyntax.AST.Data.Semantic.Base.Metadata as Meta
import qualified SLIR.HelmSyntax.AST.Data.Semantic.Base.Header   as Header

-- ++ TermLevel
import qualified SLIR.HelmSyntax.AST.Data.Semantic.TermLevel.Expr     as E
import qualified SLIR.HelmSyntax.AST.Data.Semantic.TermLevel.Patterns as P

-- ++ TopLevel
import qualified SLIR.HelmSyntax.AST.Data.Semantic.TopLevel.Fixities  as Decl
import qualified SLIR.HelmSyntax.AST.Data.Semantic.TopLevel.Functions as Decl
import qualified SLIR.HelmSyntax.AST.Data.Semantic.TopLevel.Unions    as Decl

-- + Local
import qualified SLIR.HelmSyntax.Program.Core.Lift.Data.System as Sys
import qualified SLIR.HelmSyntax.Program.Core.Lift.Data.Report as Report
-- *



{-# ANN module ("HLint: ignore" :: String) #-}







-- | Utils
--

freshIdent = do
    idx <- Sys.incCounter
    
    return $ Fresh.freshIdent idx


freshBinder = do
    idx <- Sys.incCounter
    
    return $ Fresh.freshBinder idx


newDecl :: ID.Ident -> [Etc.Binder] -> E.Expr -> Sys.Lift ()
newDecl name args expr =
    M.tell [fn]
    where
        fn = Decl.Function (Etc.Binder_ name) args expr Decl.Unknown Meta.Empty




-- | Syntax Traversals
--
liftDecls :: [Decl.Function] -> Sys.Lift [Decl.Function]
liftDecls decls = M.mapM liftDecl decls

liftDecl :: Decl.Function -> Sys.Lift Decl.Function
liftDecl fn@(SudoFFI.isSuperFFI -> True) = return fn
liftDecl (Decl.Function name args expr sig meta) = do
    expr' <- liftExpr expr
    
    return (Decl.Function name args expr' sig meta)


liftExpr :: E.Expr -> Sys.Lift E.Expr
liftExpr var@(E.Var name meta) = do
    return var

liftExpr (E.Lit val meta) = do
    return (E.Lit val meta)

liftExpr (E.Tuple items meta) = do
    items' <- M.mapM liftExpr items
    
    return (E.Tuple items' meta)

liftExpr (E.List xs meta) = do
    xs' <- M.mapM liftExpr xs
    
    return (E.List xs' meta)

liftExpr (E.Constr ident meta) = do
    return (E.Constr ident meta)

liftExpr (E.InfixApp sym e1 e2 meta) = do
    e1' <- liftExpr e1
    e2' <- liftExpr e2
    
    return (E.InfixApp sym e1' e2' meta)

liftExpr (E.If intros elseExpr meta) = do
    intros' <- Core.mapPairsM liftExpr intros
    elseExpr' <- liftExpr elseExpr
    
    return (E.If intros' elseExpr' meta)

liftExpr (E.Let fns expr meta) = do
    return (E.Let fns expr meta)

liftExpr (E.Case con alts meta) = do
    con' <- liftExpr con
    alts' <- M.mapM liftCaseAlt alts
    
    return (E.Case con' alts' meta)

liftExpr (E.Parens expr meta) = do
    expr' <- liftExpr expr
    
    return (E.Parens expr' meta)

liftExpr (E.App e1 e2 meta) = do
    e1' <- liftExpr e1
    e2' <- liftExpr e2
    
    return (E.App e1' e2' meta)


liftExpr (E.FunCall name args ty meta) = do
    return (E.FunCall name args ty meta)

liftExpr (E.ConCall name args ty meta) = do
    return (E.ConCall name args ty meta)

liftExpr (E.Abs arg expr meta) = do
    ident <- freshIdent
    expr' <- liftExpr expr
    -- *
    
    -- *
    let freeVars = map Etc.Binder_ $ Expr.freeVars expr'
    -- *
    
    -- *
    -- let args = Binder.voidBinderTypes $ Set.toList $ Set.fromList $ map Etc.Binder_ freeVars
    let args = Set.toList
             $ Set.fromList
             $ Binder.voidBinderTypes
             $ (arg : freeVars)
    -- *
    
    -- *
    newDecl ident args expr'
    -- *
    
    -- *
    let exprArgs = (Binder.voidBinderTypes freeVars) `Scope.without` [(Binder.voidBinderTypes arg)]
    -- *
    
    -- *
    let newExpr = Fold.foldl E.App' (E.Var' ident) (toVars exprArgs)
    -- *
    
    -- *
    -- return (E.Var' ident)
    return newExpr


-- liftExpr (E.Abs arg expr meta) = do
--     ident <- freshIdent
--     expr' <- liftExpr expr
--     -- *
-- 
--     -- *
--     let freeVars = map Etc.Binder_ $ Expr.freeVars expr'
--     -- *
-- 
--     -- *
--     let newArgs = Set.toList $ Set.fromList (Binder.voidBinderTypes arg : freeVars)
--     -- *
-- 
--     -- *
--     newDecl ident newArgs expr'
--     -- *
-- 
--     -- *
--     let newExpr = E.FunCall_ ident (map toVar newArgs)
--     -- *
-- 
--     -- *
--     return (E.Var' ident)
-- 
--     where
--         -- (args', expr1) = Expr.hoistLambdas expr
-- 
--         toVar (Etc.Binder_ ident) = E.Var' ident


-- | Case alternatives
--
liftCaseAlt :: P.CaseAlt -> Sys.Lift P.CaseAlt
liftCaseAlt (P.CaseAlt patrn expr meta) = do
    patrn' <- liftPattern patrn
    expr' <- liftExpr expr
    
    return (P.CaseAlt patrn' expr' meta)





-- | Patterns
--
liftPattern :: P.Pattern -> Sys.Lift P.Pattern
liftPattern (P.Lit lit meta) = do
    return (P.Lit lit meta)

liftPattern (P.List xs meta) = do
    xs' <- M.mapM liftPattern xs
    
    return (P.List xs' meta)

liftPattern (P.ListCons xs Nothing meta) = do
    xs' <- M.mapM liftPattern xs
    
    return (P.ListCons xs' Nothing meta)


liftPattern (P.ListCons xs (Just end) meta) = do
    xs' <- M.mapM liftPattern xs
    end' <- liftPattern end
    
    return (P.ListCons xs' (Just end') meta)

liftPattern (P.Tuple items meta) = do
    items' <- M.mapM liftPattern items
    
    return (P.Tuple items' meta)

liftPattern (P.Constr ident args meta) = do
    args' <- M.mapM liftPattern args
    
    return (P.Constr ident args' meta)

liftPattern (P.Var ident meta) = do
    return (P.Var ident meta)

liftPattern (P.Wildcard meta) = do
    return (P.Wildcard meta)




-- | Misc. Internal Helpers
--

toVar :: Etc.Binder -> E.Expr
toVar (Etc.Binder_ ident) = E.Var' ident

toVars :: [Etc.Binder] -> [E.Expr]
toVars = map toVar


