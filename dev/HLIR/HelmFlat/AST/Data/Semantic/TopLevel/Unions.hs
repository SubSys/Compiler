{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module HLIR.HelmFlat.AST.Data.Semantic.TopLevel.Unions (
    IR.Union
  , IR.Constructor
    
  , pattern Union
  , pattern Constructor
) where


-- *
import Core

--- Local
import qualified HLIR.HelmFlat.Internal.AST as IR
-- *




pattern Union :: IR.Ident
              -> [IR.Ident]
              -> [IR.Constructor]
              -> IR.Meta
              -> IR.Union

pattern Union name args valueConstructors meta = IR.Union name args valueConstructors meta


pattern Constructor :: IR.Ident -> [IR.Type] -> IR.Meta -> IR.Constructor
pattern Constructor name args meta = IR.Constructor name args meta


