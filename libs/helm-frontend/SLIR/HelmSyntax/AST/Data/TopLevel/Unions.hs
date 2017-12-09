{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module SLIR.HelmSyntax.AST.Data.TopLevel.Unions (
      IR.Union
    , IR.Constructor
    
    , pattern Union
    , pattern Constructor
) where


-- *
import Core

--- Local
import qualified SLIR.HelmSyntax.Internal.AST as IR

-- ~ AST - Essential Instances
import SLIR.HelmSyntax.Internal.AST.Instances.Essential ()
-- *


pattern Union :: IR.Big Text
              -> [IR.Low Text]
              -> [IR.Constructor]
              -> Maybe IR.Meta
              -> IR.Union

pattern Union name args valueConstructors optMeta = IR.Union name args valueConstructors optMeta


pattern Constructor :: IR.Big Text -> [IR.Type] -> Maybe IR.Meta -> IR.Constructor
pattern Constructor name args optMeta = IR.Constructor name args optMeta

