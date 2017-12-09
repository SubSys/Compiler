{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module CGIR.Metal.AST.TopLevel.Decls.Header.Parameters (
      IR.TemplateDecl(..)
    , IR.TemplateArg(..)
    
    -- | Function Parameters
    --
    , IR.FunctionSpecifier(..)
    , IR.Input(..)
    , IR.Output(..)
    
    -- See section `4.3.1` of the 'Metal Shading Language Specification (V2)'.
    , IR.ArgAttribute(..)
) where


-- *
import Core

--- Local
import qualified CGIR.Metal.Internal.AST as IR
-- *