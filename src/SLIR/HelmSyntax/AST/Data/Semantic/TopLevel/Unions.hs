{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module SLIR.HelmSyntax.AST.Data.Semantic.TopLevel.Unions (
    IR.Union
  , IR.Constructor
    
  , pattern Union
  , pattern Constructor
) where


-- *
import Core

--- Local
import qualified SLIR.HelmSyntax.Internal.AST as IR
-- *




pattern Union :: IR.Ident
              -> [IR.Ident]
              -> [IR.Constructor]
              -> IR.Meta
              -> IR.Union

pattern Union name args valueConstructors meta = IR.Union name args valueConstructors meta


pattern Constructor :: IR.Ident -> [IR.Type] -> IR.Meta -> IR.Constructor
pattern Constructor name args meta = IR.Constructor name args meta


