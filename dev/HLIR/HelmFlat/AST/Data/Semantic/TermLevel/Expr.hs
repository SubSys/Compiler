{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module HLIR.HelmFlat.AST.Data.Semantic.TermLevel.Expr (
    IR.Expr
  , pattern Lit
  , pattern Tuple
  , pattern List
  , pattern Constr
  , pattern Case
  , pattern FunCall
  , pattern ConCall
) where


-- *
import Core

--- Local
import qualified HLIR.HelmFlat.Internal.AST as IR
-- *




pattern Lit ::  IR.LiteralValue -> IR.Expr
pattern Lit val = IR.LitExpr val

pattern Tuple :: [IR.Expr] -> IR.Expr
pattern Tuple items = IR.TupleExpr items

pattern List :: [IR.Expr] -> IR.Expr
pattern List xs = IR.ListExpr xs


-- |
-- A data constructor
--
pattern Constr :: IR.Ident -> IR.Expr
pattern Constr ident = IR.ConstrExpr ident


-- |
-- A case expression.
--
pattern Case :: IR.Expr -> [IR.CaseAlt] -> IR.Expr
pattern Case con alts = IR.CaseExpr con alts


-- | Uncurried version of expression application.
--
pattern FunCall :: IR.Ref -> [IR.Expr] -> IR.Expr
pattern FunCall name args = IR.FunCallExpr name args


-- | Uncurried version of value constructor (I.e. `E.Constr`) application
--
pattern ConCall :: IR.Ident -> [IR.Expr] -> IR.Expr
pattern ConCall name args = IR.ConCallExpr name args



