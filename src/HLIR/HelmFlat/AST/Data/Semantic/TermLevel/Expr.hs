{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module HLIR.HelmFlat.AST.Data.Semantic.TermLevel.Expr (
    IR.Expr
  , pattern Lit
  , pattern Tuple
  , pattern List
  , pattern Case
  , pattern FunCall
  , pattern ConCall
  , pattern Ref
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
-- A case expression.
--
pattern Case :: IR.Expr -> [IR.CaseAlt] -> IR.Expr
pattern Case con alts = IR.CaseExpr con alts


-- | Uncurried version of expression application.
--
pattern FunCall :: IR.Ident -> [IR.Expr] -> IR.Expr
pattern FunCall name args = IR.FunCallExpr (IR.Ref name) args


-- | A data constructor
-- Is Either:
-- * An uncurried version of value constructor (I.e. `E.Constr`) application
-- * A data constructor
--
pattern ConCall :: IR.Ident -> [IR.Expr] -> IR.Expr
pattern ConCall name args = IR.ConCallExpr name args


-- | This constructor is for base values.
--
pattern Ref :: IR.Ident -> IR.Expr
pattern Ref name = IR.RefExpr (IR.Ref name)


