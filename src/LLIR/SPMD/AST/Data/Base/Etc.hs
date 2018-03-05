{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module LLIR.SPMD.AST.Data.Base.Etc (
    IR.Input
  , IR.ParameterQualifier
    
  , pattern Input
  , pattern Input_
  
  , pattern In
  , pattern Out
  , pattern Inout
) where


-- ~
import Core
import qualified LLIR.SPMD.Internal.AST as IR
-- ~



pattern Input :: Maybe IR.ParameterQualifier -> IR.Type -> IR.Ident -> IR.Input
pattern Input qualifier ty name = IR.Input qualifier ty name



-- | Parameter Qualifiers
--


pattern In :: IR.ParameterQualifier
pattern In = IR.InParameterQualifier

pattern Out :: IR.ParameterQualifier
pattern Out = IR.OutParameterQualifier

pattern Inout :: IR.ParameterQualifier
pattern Inout = IR.InoutParameterQualifier






-- | Alternate variations - convenience helpers
--


pattern Input_ :: IR.Type -> IR.Ident -> IR.Input
pattern Input_ ty name = IR.Input Nothing ty name









