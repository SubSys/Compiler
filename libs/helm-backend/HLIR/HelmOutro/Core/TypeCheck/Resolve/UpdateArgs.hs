{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns #-}
module HLIR.HelmOutro.Core.TypeCheck.Resolve.UpdateArgs (
    updateDecl
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
import qualified Text.Show.Prettyprint as PP


--- Local Deps
-- ~ HelmOutro IR
import qualified HLIR.HelmOutro.Data.Payload as Payload

-- ~ HelmOutro AST
-- ~~ Base
import qualified HLIR.HelmOutro.AST.Base.Ident  as ID
import qualified HLIR.HelmOutro.AST.Base.Types  as T
import qualified HLIR.HelmOutro.AST.Base.Values as V
import qualified HLIR.HelmOutro.AST.Base.Etc    as Etc
-- ~~ TermLevel
import qualified HLIR.HelmOutro.AST.TermLevel.Expressions as E
import qualified HLIR.HelmOutro.AST.TermLevel.Patterns    as P
-- ~~ TopLevel
import qualified HLIR.HelmOutro.AST.TopLevel.Functions as Decl
import qualified HLIR.HelmOutro.AST.TopLevel.Unions    as Decl


--- Local
import qualified HLIR.HelmOutro.Core.TypeCheck.Data.Env                    as Env
import qualified HLIR.HelmOutro.Core.TypeCheck.Data.Report                 as Report
import qualified HLIR.HelmOutro.Core.TypeCheck.Data.Subst                  as Sub
import qualified HLIR.HelmOutro.Core.TypeCheck.Data.System                 as Sys
import qualified HLIR.HelmOutro.Core.TypeCheck.Data.TypeSystem             as TS
import qualified HLIR.HelmOutro.Core.TypeCheck.Data.Canonical.Ident        as CID
import qualified HLIR.HelmOutro.Core.TypeCheck.Data.System.Scope           as Scope
import qualified HLIR.HelmOutro.Core.TypeCheck.Data.Unification.Solver     as Solver
import qualified HLIR.HelmOutro.Core.TypeCheck.Data.Unification.Constraint as Con

import qualified HLIR.HelmOutro.Core.TypeCheck.Sudo.FFI.Helpers as SudoFFI
-- *




updateArgsSudoFFI :: Decl.Function -> Decl.Function
updateArgsSudoFFI (Decl.Function name [] expr sig@(Just (T.Forall _ t))) =
    let (args', expr') = splitArgs expr
        inputTypes = List.init $ flattenTypes t

        -- Resulting Data (Finished)
        tyArgs = map update $ List.zip inputTypes args'
    in
        Decl.Function name tyArgs expr' sig

    where
        splitArgs :: E.Expr -> ([Etc.Arg], E.Expr)
        splitArgs (E.Abs bs e) =
            let (bs', e') = splitArgs e
            in
                (bs ++ bs', e')
        
        splitArgs x = ([], x)
        
        
        -- | flatten types
        --
        flattenTypes :: T.Type -> [T.Type]
        flattenTypes (T.Arr t1 t2) =
            t1 : flattenTypes t2
        flattenTypes x = [x]
        
        
        update :: (T.Type, Etc.Arg) -> Etc.Arg
        update (ty, Etc.Arg b Nothing) =
            Etc.Arg b (Just ty)

-- TODO: Tmp...
updateArgsSudoFFI (Decl.Function name args expr sig@Just{}) =
    let expr' = Fold.foldr absExpr expr args
        decl = Decl.Function name [] expr' sig
    in
        updateArgsSudoFFI decl
    
    where
        absExpr :: Etc.Arg -> E.Expr -> E.Expr
        absExpr (Etc.Arg b _) =
            E.Abs [Etc.Arg b Nothing]
        


updateDecl ::  Sub.Subst -> Decl.Function -> Decl.Function
updateDecl _  decl@(SudoFFI.ifSudoFFI -> True) = updateArgsSudoFFI decl
    
updateDecl subs (Decl.Function name args expr scheme) =
    let args' = map (updateArg subs) args
        expr' = updateExpr subs expr
    in
        Decl.Function
            name
            args'
            expr'
            scheme



updateExpr :: Sub.Subst -> E.Expr -> E.Expr
updateExpr subs expr@E.Var{} = expr
updateExpr subs expr@E.Lit{} = expr
updateExpr subs expr@E.Con{} = expr

updateExpr subs (E.Record xs) =
    let updateField (n, e) = (n, updateExpr subs e)
    in
        E.Record $ map updateField xs

updateExpr subs (E.Tuple xs) =
    E.Tuple $ map (updateExpr subs) xs

updateExpr subs (E.List xs) =
    E.List $ map (updateExpr subs) xs

updateExpr subs (E.Let fns e) =
    E.Let fns (updateExpr subs e)
        
updateExpr subs (E.Case con alts ) =
    E.Case
        (updateExpr subs con)
        (map (updateCaseAlt subs) alts)

updateExpr subs (E.App e1 e2 ) =
    E.App
        (updateExpr subs e1)
        (updateExpr subs e2)

updateExpr subs (E.Abs args e) =
    E.Abs
        (map (updateArg subs) args)
        (updateExpr subs e)

updateExpr subs (E.FunCall ref params) =
    E.FunCall
        ref
        (map (updateExpr subs) params)

updateExpr subs (E.ConCall con params) =
    E.ConCall
        con
        (map (updateExpr subs) params)



updateCaseAlt :: Sub.Subst -> P.CaseAlt -> P.CaseAlt
updateCaseAlt subs (P.CaseAlt patrn e) =
    P.CaseAlt patrn (updateExpr subs e)



-- *
-- | Core Logic
-- *

updateArg :: Sub.Subst -> Etc.Arg -> Etc.Arg
updateArg subs (Etc.Arg name (Just ty)) =
    let
        -- (T.Forall as ty') = TS.closeOver $ Sub.apply subs ty
        ty' = Sub.apply subs ty
    in
        -- if List.length as >= 2 then
        --     -- error $ PP.prettyShow as
        --     error $ PP.prettyShow (Sub.apply subs ty)
        -- else
            Etc.Arg name (Just ty')

    where
        isVar :: T.Type -> Bool
        isVar T.Var{} = True
        isVar _ = False





