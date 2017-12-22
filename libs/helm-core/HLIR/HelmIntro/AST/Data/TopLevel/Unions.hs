{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module HLIR.HelmIntro.AST.Data.TopLevel.Unions (
      IR.Union
    , IR.Constructor
    
    , pattern Union
    , pattern Constructor
) where


-- *
import Core

--- Local
import qualified HLIR.HelmIntro.Internal.AST as IR
-- *


pattern Union :: IR.Big
              -> [IR.Low]
              -> [IR.Constructor]
              -> IR.Union

pattern Union name args valueConstructors = IR.Union name args valueConstructors


pattern Constructor :: IR.Big -> [IR.Type] ->  IR.Constructor
pattern Constructor name args = IR.Constructor name args

