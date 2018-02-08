{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternGuards #-}


-- | Update Binders & Metadata fields with inferred type.
--
module SLIR.HelmSyntax.Module.Core.TypeCheck.Inference.Engine.Update.Binders (
    updateBinders
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
import qualified SLIR.HelmSyntax.AST.Utils.Scope                         as Scope
import qualified SLIR.HelmSyntax.AST.Utils.Auxiliary.Ident               as ID
import qualified SLIR.HelmSyntax.AST.Utils.Auxiliary.Functions.SudoFFI   as SudoFFI
import qualified SLIR.HelmSyntax.AST.Utils.Auxiliary.Functions.Recursive as Rec
import qualified SLIR.HelmSyntax.AST.Utils.Auxiliary.Type                as T

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
import qualified SLIR.HelmSyntax.Module.Core.TypeCheck.Inference.Data.System      as Sys
import qualified SLIR.HelmSyntax.Module.Core.TypeCheck.Data.Report                as Report
import qualified SLIR.HelmSyntax.Module.Core.TypeCheck.Inference.Data.Env         as Env
import qualified SLIR.HelmSyntax.Module.Core.TypeCheck.Inference.Utils.TypeSystem as TS
import qualified SLIR.HelmSyntax.Module.Core.TypeCheck.Inference.Syntax.Scope     as Scope
import qualified SLIR.HelmSyntax.Module.Core.TypeCheck.Solver.Data.Constraint     as Con
import qualified SLIR.HelmSyntax.Module.Core.TypeCheck.Solver.Engine              as Solver
import qualified SLIR.HelmSyntax.Module.Core.TypeCheck.Subst.Types                as TySub
-- *




updateBinders :: TySub.Subst -> Decl.Function -> Decl.Function
updateBinders = traverseDecl

traverseExpr :: TySub.Subst -> E.Expr -> E.Expr
traverseDecl :: TySub.Subst -> Decl.Function -> Decl.Function
traversePattern :: TySub.Subst -> P.Pattern -> P.Pattern



-- traverseDecl subs

traverseDecl subs fn@(SudoFFI.isSuperFFI -> True) = fn

traverseDecl subs (Decl.Function (Etc.Binder name _) args expr sig@(Decl.Validated (T.Forall _ ty) _) meta) =
    Decl.Function
        (Etc.Binder name (Just ty))
        (map (updateBinder subs) args)
        (traverseExpr subs expr)
        sig
        (updateMeta subs meta)


traverseExpr subs (E.Var name meta) =
    E.Var name (updateMeta subs meta)

traverseExpr subs (E.Lit val meta) =
    E.Lit val (updateMeta subs meta)

traverseExpr subs (E.Tuple items meta) =
    E.Tuple (map (traverseExpr subs) items) (updateMeta subs meta)

traverseExpr subs (E.List xs meta) =
    E.List (map (traverseExpr subs) xs) (updateMeta subs meta)

traverseExpr subs (E.Constr ident meta) =
    E.Constr ident (updateMeta subs meta)

traverseExpr subs (E.InfixApp sym e1 e2 meta) =
    E.InfixApp sym
        (traverseExpr subs e1)
        (traverseExpr subs e2)
        (updateMeta subs meta)

traverseExpr subs (E.If intros elseExpr meta) =
    E.If
        (Core.mapPairs (traverseExpr subs) intros)
        (traverseExpr subs elseExpr)
        (updateMeta subs meta)

traverseExpr subs (E.Let fns expr meta) =
    E.Let
        fns -- Don't process `fns`!
        (traverseExpr subs expr)
        (updateMeta subs meta)

traverseExpr subs (E.Case con alts meta) =
    E.Case
        con
        (map (traverseCaseAlt subs) alts)
        (updateMeta subs meta)

traverseExpr subs (E.Parens expr meta) =
    E.Parens
        (traverseExpr subs expr)
        (updateMeta subs meta)

traverseExpr subs (E.App e1 e2 meta) =
    E.App
        (traverseExpr subs e1)
        (traverseExpr subs e2)
        (updateMeta subs meta)

traverseExpr subs (E.Abs arg expr meta) =
    E.Abs
        (updateBinder subs arg)
        (traverseExpr subs expr)
        (updateMeta subs meta)




traverseExpr subs (E.FunCall ident args (Just ty) meta) =
    E.FunCall
        ident
        (map (traverseExpr subs) args)
        (Just $ TySub.apply subs ty)
        (updateMetaWithOLType subs meta)


traverseExpr subs (E.ConCall ident args (Just ty) meta) =
    E.ConCall
        ident
        (map (traverseExpr subs) args)
        (Just $ TySub.apply subs ty)
        (updateMeta subs meta)


traverseExpr subs x = error $ PP.prettyShow x



traverseCaseAlt subs (P.CaseAlt patrn expr meta) =
    P.CaseAlt
        (traversePattern subs patrn)
        (traverseExpr subs expr)
        (updateMeta subs meta)



traversePattern subs (P.Lit lit meta) =
    P.Lit lit (updateMeta subs meta)

traversePattern subs (P.List xs meta) =
    P.List
        (map (traversePattern subs) xs)
        (updateMeta subs meta)

traversePattern subs (P.ListCons xs Nothing meta) =
    P.ListCons
        (map (traversePattern subs) xs)
        Nothing
        (updateMeta subs meta)

traversePattern subs (P.ListCons xs (Just end) meta) =
    P.ListCons
        (map (traversePattern subs) xs)
        (Just $ traversePattern subs end)
        (updateMeta subs meta)

traversePattern subs (P.Tuple items meta) =
    P.Tuple
        (map (traversePattern subs) items)
        (updateMeta subs meta)

traversePattern subs (P.Constr ident args meta) =
    P.Constr
        ident
        (map (traversePattern subs) args)
        (updateMeta subs meta)

traversePattern subs (P.Var ident meta) =
    P.Var
        (updateBinder subs ident)
        (updateMeta subs meta)

traversePattern subs (P.Wildcard meta) =
    P.Wildcard meta









-- | Core Logic
--

updateBinder :: TySub.Subst -> Etc.Binder -> Etc.Binder
updateBinder subs (Etc.Binder name (isVar -> (Just ty))) =
    let
        resType = TySub.apply subs ty
    in
        Etc.Binder name (Just resType)

updateBinder _ x = x


isVar :: Maybe T.Type -> Maybe T.Type
isVar (Just (x@T.Var{})) = Just x
isVar _ = Nothing





updateMeta :: TySub.Subst -> Meta.Meta -> Meta.Meta
updateMeta subs x@Meta.Meta{}
    | Just tv <- Meta.inferredType x =
        Meta.Meta
            { Meta.span = Meta.span x
            , Meta.inferredType = Just (TySub.apply subs tv)
            , Meta.overloadedTargetType = Meta.overloadedTargetType x
            , Meta.originalNamespace = Nothing
            }

updateMeta subs x = x



updateMetaWithOLType subs x@Meta.Meta{}
    | Just tv <- Meta.inferredType x
    , Just olTv <- Meta.overloadedTargetType x =
        Meta.Meta
            { Meta.span = Meta.span x
            , Meta.inferredType = Just (TySub.apply subs tv)
            , Meta.overloadedTargetType = Just (TySub.apply subs olTv)
            , Meta.originalNamespace = Nothing
            }

updateMetaWithOLType subs x = x


-- | Internal Helpers
--


-- synthesizeFFIArgBinders subs fn@(Decl.Function name args expr sig meta)
--     | Right scheme@(T.Forall _ ty) <- SudoFFI.ifSudoDeclGetTy fn =
--         let
--             returnType = T.getReturnType ty
--             inputTypes = T.getInputTypes ty
--         in
-- 
--     | Left ty <- SudoFFI.ifSudoDeclGetTy fn =
--         error "tes"



