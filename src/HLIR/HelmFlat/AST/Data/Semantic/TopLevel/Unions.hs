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
              -> IR.Union

pattern Union name args valueConstructors = IR.Union name args valueConstructors


pattern Constructor :: IR.Ident -> [IR.Type] -> IR.Constructor
pattern Constructor name args = IR.Constructor name args


