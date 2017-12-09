{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module CGIR.Metal.AST.Base.Ident (
      IR.Low(..)
    , IR.Big(..)
    , IR.ScopePath(..)
    , IR.ValuePath(..)
    , IR.QSeg
    , pattern BigSeg
    , pattern LowSeg
) where


-- *
import Core

--- Local
import qualified CGIR.Metal.Internal.AST as IR
-- *


pattern BigSeg :: IR.Big -> IR.QSeg
pattern BigSeg id_ = IR.BigSeg id_

pattern LowSeg :: IR.Low -> IR.QSeg
pattern LowSeg id_ = IR.LowSeg id_




