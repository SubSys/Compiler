{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}

-- | ‘Identifier & Scope Related Constructors’ -are organized here
--
-- Current Standards/Conventions (simply for consistency):
-- * Import into the `ID` namespace, E.g.
--   `import qualified SLIR.HelmSyntax.AST.Data.Base.Ident as ID`.
--
module SLIR.HelmSyntax.AST.Data.Base.Ident (
      IR.Low
    , IR.Big
    , IR.Sym
    , IR.Namespace(..)
    
    , pattern Low
    , pattern Big
    , pattern Sym
    
    , pattern Low'
    , pattern Big'
    , pattern Sym'
) where


-- *
import Core

--- Local
import qualified SLIR.HelmSyntax.Internal.AST as IR

-- ~ AST - Essential Instances
import SLIR.HelmSyntax.Internal.AST.Instances.Essential ()
-- *



pattern Low :: Text -> Maybe IR.Namespace -> Maybe IR.Meta -> IR.Low
pattern Low id' ns metaOpt = IR.Low id' ns metaOpt

pattern Big :: Text -> Maybe IR.Namespace -> Maybe IR.Meta -> IR.Big
pattern Big id' ns metaOpt = IR.Big id' ns metaOpt

pattern Sym :: Text -> Maybe IR.Namespace -> Maybe IR.Meta -> IR.Sym
pattern Sym id' ns metaOpt = IR.Sym id' ns metaOpt



pattern Low :: Text -> Maybe IR.Namespace -> IR.Low
pattern Low' id' ns = IR.Low id' ns Nothing

pattern Big :: Text -> Maybe IR.Namespace -> IR.Big
pattern Big' id' ns = IR.Big id' ns Nothing

pattern Sym :: Text -> Maybe IR.Namespace -> IR.Sym
pattern Sym' id' ns = IR.Sym id' ns Nothing