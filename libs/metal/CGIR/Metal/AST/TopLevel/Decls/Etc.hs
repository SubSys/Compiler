{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module CGIR.Metal.AST.TopLevel.Decls.Etc (
      IR.Block
    , IR.Namespace
    , pattern Block
    , pattern Namespace
) where


-- *
import Core

--- Local
import qualified CGIR.Metal.Internal.AST as IR
-- *



-- *
-- | Block & Namespace Declarations
-- *
pattern Block :: [IR.Stmt] -> IR.Block
pattern Block body = IR.Block body


pattern Namespace :: IR.Big -> IR.Block -> IR.Namespace
pattern Namespace name body = IR.Namespace name body

