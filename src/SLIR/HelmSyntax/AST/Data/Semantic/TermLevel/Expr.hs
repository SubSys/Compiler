{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module SLIR.HelmSyntax.AST.Data.Semantic.TermLevel.Expr (
    IR.Expr
  , pattern Var
  , pattern Lit
  , pattern Tuple
  , pattern List
  , pattern Constr
  , pattern InfixApp
  , pattern If
  , pattern Let
  , pattern Case
  , pattern Parens
  , pattern App
  , pattern Abs
  , pattern FunCall
  , pattern ConCall
    
  , pattern Var'
  , pattern Lit'
  , pattern Tuple'
  , pattern List'
  , pattern Constr'
  , pattern InfixApp'
  , pattern If'
  , pattern Let'
  , pattern Case'
  , pattern Parens'
  , pattern App'
  , pattern Abs'
  , pattern FunCall'
  , pattern ConCall'
  
  , pattern FunCall_
  , pattern ConCall_
) where


-- *
import Core

--- Local
import qualified SLIR.HelmSyntax.Internal.AST as IR
-- *






pattern Var :: IR.Ident -> IR.Meta -> IR.Expr
pattern Var name meta = IR.VarExpr name meta


pattern Lit ::  IR.LiteralValue -> IR.Meta -> IR.Expr
pattern Lit val meta = IR.LitExpr val meta

pattern Tuple :: [IR.Expr] -> IR.Meta -> IR.Expr
pattern Tuple items meta = IR.TupleExpr items meta

pattern List :: [IR.Expr] -> IR.Meta -> IR.Expr
pattern List xs meta = IR.ListExpr xs meta


-- |
-- A data constructor
--
pattern Constr :: IR.Ident -> IR.Meta -> IR.Expr
pattern Constr ident meta = IR.ConstrExpr ident meta

pattern InfixApp :: IR.Ident -> IR.Expr -> IR.Expr -> IR.Meta -> IR.Expr
pattern InfixApp sym e1 e2 meta = IR.InfixAppExpr sym e1 e2 meta


pattern If :: [(IR.Expr, IR.Expr)] -> IR.Expr -> IR.Meta -> IR.Expr
pattern If intros elseExpr meta = IR.IfExpr intros elseExpr meta



-- |
-- A let binding
--
pattern Let :: [IR.Function] -> IR.Expr -> IR.Meta -> IR.Expr
pattern Let fns expr meta = IR.LetExpr fns expr meta


-- |
-- A case expression.
--
pattern Case :: IR.Expr -> [IR.CaseAlt] -> IR.Meta -> IR.Expr
pattern Case con alts meta = IR.CaseExpr con alts meta


pattern Parens :: IR.Expr -> IR.Meta -> IR.Expr
pattern Parens expr meta = IR.ParensExpr expr meta


pattern App :: IR.Expr -> IR.Expr -> IR.Meta -> IR.Expr
pattern App e1 e2 meta = IR.AppExpr e1 e2 meta


pattern Abs :: IR.Binder -> IR.Expr -> IR.Meta -> IR.Expr
pattern Abs arg expr meta = IR.AbsExpr arg expr meta


-- | Uncurried version of expression application.
--
pattern FunCall :: IR.Ident -> [IR.Expr] -> Maybe IR.Type -> IR.Meta -> IR.Expr
pattern FunCall name args ty meta = IR.FunCallExpr name args ty meta


-- | Uncurried version of value constructor (I.e. `E.Constr`) application
--
pattern ConCall :: IR.Ident -> [IR.Expr] -> Maybe IR.Type -> IR.Meta -> IR.Expr
pattern ConCall name args ty meta = IR.ConCallExpr name args ty meta



-- | Alternative variations, or helpers.
--

pattern Var' :: IR.Ident -> IR.Expr
pattern Var' ident <- IR.VarExpr ident _
    where
        Var' ident = IR.VarExpr ident IR.Empty

pattern Lit' ::  IR.LiteralValue -> IR.Expr
pattern Lit' val <- IR.LitExpr val _
    where
        Lit' val = IR.LitExpr val IR.Empty

pattern Tuple' :: [IR.Expr] -> IR.Expr
pattern Tuple' items <- IR.TupleExpr items _
    where
        Tuple' items = IR.TupleExpr items IR.Empty

pattern List' :: [IR.Expr] -> IR.Expr
pattern List' xs <- IR.ListExpr xs _
    where
        List' xs = IR.ListExpr xs IR.Empty


pattern Constr' :: IR.Ident -> IR.Expr
pattern Constr' ident <- IR.ConstrExpr ident _
    where
        Constr' ident = IR.ConstrExpr ident IR.Empty

pattern InfixApp' :: IR.Ident -> IR.Expr -> IR.Expr -> IR.Expr
pattern InfixApp' sym e1 e2 <- IR.InfixAppExpr sym e1 e2 _
    where
        InfixApp' sym e1 e2 = IR.InfixAppExpr sym e1 e2 IR.Empty

pattern If' :: [(IR.Expr, IR.Expr)] -> IR.Expr -> IR.Expr
pattern If' intros elseExpr <- IR.IfExpr intros elseExpr _
    where
        If' intros elseExpr = IR.IfExpr intros elseExpr IR.Empty


pattern Let' :: [IR.Function] -> IR.Expr -> IR.Expr
pattern Let' fns expr <- IR.LetExpr fns expr _
    where
        Let' fns expr = IR.LetExpr fns expr IR.Empty

pattern Parens' :: IR.Expr -> IR.Expr
pattern Parens' expr <- IR.ParensExpr expr _
    where
        Parens' expr = IR.ParensExpr expr IR.Empty


pattern App' :: IR.Expr -> IR.Expr -> IR.Expr
pattern App' e1 e2 <- IR.AppExpr e1 e2 _
    where
        App' e1 e2 = IR.AppExpr e1 e2 IR.Empty

pattern Abs' :: IR.Binder -> IR.Expr -> IR.Expr
pattern Abs' arg expr <- IR.AbsExpr arg expr _
    where
        Abs' arg expr = IR.AbsExpr arg expr IR.Empty

pattern Case' :: IR.Expr -> [IR.CaseAlt] -> IR.Expr
pattern Case' con alts <- IR.CaseExpr con alts _
    where
        Case' con alts = IR.CaseExpr con alts IR.Empty




pattern FunCall' :: IR.Ident -> [IR.Expr] -> Maybe IR.Type -> IR.Expr
pattern FunCall' name args ty <- IR.FunCallExpr name args ty _
    where
        FunCall' name args ty = IR.FunCallExpr name args ty IR.Empty



pattern ConCall' :: IR.Ident -> [IR.Expr] -> Maybe IR.Type -> IR.Expr
pattern ConCall' name args ty <- IR.ConCallExpr name args ty _
    where
        ConCall' name args ty = IR.ConCallExpr name args ty IR.Empty



pattern FunCall_ :: IR.Ident -> [IR.Expr] -> IR.Expr
pattern FunCall_ name args <- IR.FunCallExpr name args _ _
    where
        FunCall_ name args = IR.FunCallExpr name args Nothing IR.Empty



pattern ConCall_ :: IR.Ident -> [IR.Expr] -> IR.Expr
pattern ConCall_ name args <- IR.ConCallExpr name args _ _
    where
        ConCall_ name args = IR.ConCallExpr name args Nothing IR.Empty



