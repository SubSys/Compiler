{-# LANGUAGE NoImplicitPrelude #-}
module HLIR.HelmFlat.Core.Index.Syntax (
    traverseDecls
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


-- + Recursion Schemes & Related
import qualified Data.Functor.Foldable       as F
import qualified Data.Generics.Uniplate.Data as Uni

-- + OS APIS & Related
import qualified System.IO as SIO


-- + Dev & Debugging
import qualified Text.Show.Prettyprint as PP

-- + HelmFlat Module Interface
import qualified HLIR.HelmFlat.Program.Data.Interface as I

-- + HelmFlat AST Renderer
import qualified HLIR.HelmFlat.AST.Render.Syntax.Driver as Syntax

-- + HelmFlat AST Utils
import qualified HLIR.HelmFlat.AST.Utils.Scope           as Scope
import qualified HLIR.HelmFlat.AST.Utils.Auxiliary.Ident as ID

-- + HelmFlat AST
-- ++ Base
import qualified HLIR.HelmFlat.AST.Data.Semantic.Base.Etc      as Etc
import qualified HLIR.HelmFlat.AST.Data.Semantic.Base.Ident    as ID
import qualified HLIR.HelmFlat.AST.Data.Semantic.Base.Types    as T
import qualified HLIR.HelmFlat.AST.Data.Semantic.Base.Values   as V
import qualified HLIR.HelmFlat.AST.Data.Semantic.Base.Metadata as Meta
import qualified HLIR.HelmFlat.AST.Data.Semantic.Base.Header   as Header

-- ++ TermLevel
import qualified HLIR.HelmFlat.AST.Data.Semantic.TermLevel.Expr     as E
import qualified HLIR.HelmFlat.AST.Data.Semantic.TermLevel.Patterns as P

-- ++ TopLevel
import qualified HLIR.HelmFlat.AST.Data.Semantic.TopLevel.Fixities  as Decl
import qualified HLIR.HelmFlat.AST.Data.Semantic.TopLevel.Functions as Decl
import qualified HLIR.HelmFlat.AST.Data.Semantic.TopLevel.Unions    as Decl

-- + Local
import qualified HLIR.HelmFlat.Core.Index.Data            as Index
import qualified HLIR.HelmFlat.Core.Index.Scope.Referable as Referable
import qualified HLIR.HelmFlat.Core.Index.Scope.Bindable  as Bindable
import qualified HLIR.HelmFlat.Core.Index.Scope.Utils     as Scope
-- *



{-# ANN module "HLint: ignore" #-}


traverseDecls :: [Decl.Function] -> Index.Index [Decl.Function]
traverseDecls []  = return ([], Map.empty)

traverseDecls (fn:fns) = do
    (fn', s1) <- traverseDecl fn
    (fns', s2) <- Scope.withLocalSubst s1 (traverseDecls fns)
    
    return
        (fn' : fns', Map.union s2 s1)




traverseDecl :: Decl.Function -> Index.Index Decl.Function
traverseDecl (Decl.Function name args expr sig meta) = do
    (name', s1) <- Bindable.bindable name
    (args', s2) <- List.unzip <$> M.mapM Bindable.bindable args
    -- *
    
    -- *
    (expr', _) <- Scope.withLocalSubst (s1 `Map.union` Map.unions s2) (traverseExpr expr)
    -- *
    
    -- *
    return (Decl.Function name' args' expr' sig meta, s1)


traverseExpr :: E.Expr -> Index.Index E.Expr
traverseExpr (E.Var name meta) = do
    (name', _) <- Referable.referable name
    
    return (E.Var name' meta, Map.empty)

traverseExpr (E.Lit val meta) = do 
    return (E.Lit val meta, Map.empty)

traverseExpr (E.Tuple items meta) = do
    (items', _) <- List.unzip <$> M.mapM traverseExpr items
    
    return (E.Tuple items' meta, Map.empty)

traverseExpr (E.List xs meta) = do
    (xs', _) <- List.unzip <$> M.mapM traverseExpr xs
    
    return (E.List xs' meta, Map.empty)

traverseExpr (E.Constr ident meta) = do
    return (E.Constr ident meta, Map.empty)

traverseExpr (E.InfixApp sym e1 e2 meta) = do
    (sym', _) <- Referable.referable sym
    (e1', _) <- traverseExpr e1
    (e2', _) <- traverseExpr e2
    
    return (E.InfixApp sym' e1' e2' meta, Map.empty)

traverseExpr (E.If intros elseExpr meta) = do
    (intros', subs) <- List.unzip <$> M.mapM traverseIfBranch intros
    (elseExpr', _) <- Scope.withLocalSubst (Map.unions subs) (traverseExpr elseExpr)

    return (E.If intros' elseExpr' meta, Map.empty)

traverseExpr (E.Let fns expr meta) = do
    -- (fns', subs) <- List.unzip <$> M.mapM traverseDecl fns
    (fns', subs) <- traverseDecls fns
    (expr', _) <- Scope.withLocalSubst subs (traverseExpr expr)
    
    return (E.Let fns' expr' meta, Map.empty)

traverseExpr (E.Case con alts meta) = do
    (con', _) <- traverseExpr con
    (alts', _) <- List.unzip <$> M.mapM traverseCaseAlt alts
    
    return (E.Case con' alts' meta, Map.empty)

traverseExpr (E.Parens expr meta) = do
    (expr', _) <- traverseExpr expr
    
    return (E.Parens expr' meta, Map.empty)

traverseExpr (E.App e1 e2 meta) = do
    (e1', _) <- traverseExpr e1
    (e2', _) <- traverseExpr e2
    
    return (E.App e1' e2' meta, Map.empty)

traverseExpr (E.Abs arg expr meta) = do
    (arg', subs) <- Bindable.bindable arg
    (expr', _) <- Scope.withLocalSubst subs (traverseExpr expr)
    
    return (E.Abs arg' expr' meta, Map.empty)



traverseExpr (E.FunCall ident args ty meta) = do
    (ident', _) <- Referable.referable ident
    (args', _) <- List.unzip <$> M.mapM traverseExpr args
    
    return (E.FunCall ident' args' ty meta, Map.empty)



traverseExpr (E.ConCall ident args ty meta) = do
    (ident', _) <- Referable.referable ident
    (args', _) <- List.unzip <$> M.mapM traverseExpr args
    
    return (E.ConCall ident' args' ty meta, Map.empty)





traverseCaseAlt (P.CaseAlt patrn expr meta) = do
    (patrn', subs) <- traversePattern patrn
    (expr', _) <- Scope.withLocalSubst subs (traverseExpr expr)
    
    return (P.CaseAlt patrn' expr' meta, Map.empty)



traversePattern (P.Lit lit meta) = do
    return (P.Lit lit meta, Map.empty)

traversePattern (P.List xs meta) = do
    (xs', subs) <- List.unzip <$> M.mapM traversePattern xs
    
    return (P.List xs' meta, Map.unions subs)

traversePattern (P.ListCons xs Nothing meta) = do
    (xs', subs) <- List.unzip <$> M.mapM traversePattern xs
    
    return (P.ListCons xs' Nothing meta, Map.unions subs)

traversePattern (P.ListCons xs (Just end) meta) = do
    (xs', subs1) <- List.unzip <$> M.mapM traversePattern xs
    (end', subs2) <- traversePattern end
    
    return (P.ListCons xs' (Just end') meta, Map.unions subs1 `Map.union` subs2)

traversePattern (P.Tuple items meta) = do
    (items', subs) <- List.unzip <$> M.mapM traversePattern items
    
    return (P.Tuple items' meta, Map.unions subs)

traversePattern (P.Constr ident args meta) = do
    (args', subs) <- List.unzip <$> M.mapM traversePattern args
    
    return (P.Constr ident args' meta, Map.unions subs)

traversePattern (P.Var ident meta) = do
    (ident', subs) <- Bindable.bindable ident
    
    return (P.Var ident' meta, subs)

traversePattern (P.Wildcard meta) = do
    return (P.Wildcard meta, Map.empty)








-- | Internal Helpers
--


traverseIfBranch :: (E.Expr, E.Expr) -> Index.Index (E.Expr, E.Expr)
traverseIfBranch (con, body) = do
    (con', _) <- traverseExpr con
    (body', _) <- traverseExpr body
    
    return ((con', body'), Map.empty)






