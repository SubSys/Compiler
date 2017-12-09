{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module SLIR.HelmSyntax.AST.Data.Base.Etc (
      IR.Signature
    
    , pattern Validated
    , pattern Unresolved
    
    , pattern Validated'
    , pattern Unresolved'
    
) where


-- *
import Core

--- Local
import qualified SLIR.HelmSyntax.Internal.AST as IR

-- ~ AST - Essential Instances
import SLIR.HelmSyntax.Internal.AST.Instances.Essential ()
-- *



pattern Validated :: IR.Scheme -> Maybe IR.Meta -> IR.Signature
pattern Validated scheme meta = IR.Validated scheme meta

pattern Unresolved :: IR.Type -> Maybe IR.Meta -> IR.Signature
pattern Unresolved ty meta = IR.Unresolved ty meta





-- *
-- | Misc.
-- *
pattern Validated' :: IR.Scheme -> IR.Signature
pattern Validated' scheme = IR.Validated scheme Nothing

pattern Unresolved' :: IR.Type -> IR.Signature
pattern Unresolved' ty = IR.Unresolved ty Nothing


