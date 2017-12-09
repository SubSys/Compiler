{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module CGIR.Metal.AST.TopLevel.Decls.Function (
      IR.FunctionDecl
    , pattern Function
) where


-- *
import Core

--- Local
import qualified CGIR.Metal.Internal.AST as IR
-- *


pattern Function :: Maybe IR.TemplateDecl
                 -> Maybe IR.FunctionSpecifier
                 -> IR.Output
                 -> IR.Low
                 -> [IR.Input]
                 -> IR.Block
                 -> IR.FunctionDecl
pattern Function optTempDecl specifier returnType name args body =
    IR.FunctionDecl optTempDecl specifier returnType name args body