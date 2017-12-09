{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module SLIR.HelmSyntax.AST.Data.TopLevel.Fixities (
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

-- ~ AST - Essential Instances
import SLIR.HelmSyntax.Internal.AST.Instances.Essential ()
-- *



pattern InfixL :: IR.Sym Text -> IR.OpPrecedence -> Maybe IR.Meta -> IR.Infix
pattern InfixL sym precedence optMeta = IR.InfixL sym precedence optMeta

pattern InfixR :: IR.Sym Text -> IR.OpPrecedence -> Maybe IR.Meta -> IR.Infix
pattern InfixR sym precedence optMeta = IR.InfixR sym precedence optMeta

pattern InfixN :: IR.Sym Text -> IR.OpPrecedence -> Maybe IR.Meta -> IR.Infix
pattern InfixN sym precedence optMeta = IR.InfixN sym precedence optMeta




