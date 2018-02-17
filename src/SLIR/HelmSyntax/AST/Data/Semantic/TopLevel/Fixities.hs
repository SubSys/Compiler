{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module SLIR.HelmSyntax.AST.Data.Semantic.TopLevel.Fixities (
    IR.Infix
  , IR.OpPrecedence(..)

  , pattern InfixL
  , pattern InfixR
  , pattern InfixN
) where


-- *
import Core

--- Local
import qualified SLIR.HelmSyntax.Internal.AST as IR
-- *



pattern InfixL :: IR.Ident -> IR.OpPrecedence -> IR.Meta -> IR.Infix
pattern InfixL sym precedence meta = IR.InfixL sym precedence meta

pattern InfixR :: IR.Ident -> IR.OpPrecedence -> IR.Meta -> IR.Infix
pattern InfixR sym precedence meta = IR.InfixR sym precedence meta

pattern InfixN :: IR.Ident -> IR.OpPrecedence -> IR.Meta -> IR.Infix
pattern InfixN sym precedence meta = IR.InfixN sym precedence meta

















