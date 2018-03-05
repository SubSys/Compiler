{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module LLIR.SPMD.AST.Data.TopLevel.Objects (
    IR.Object
  , IR.StorageQualifier
  , IR.InterpolationQualifier

  , pattern Object
  , pattern Const
  , pattern In
  , pattern CentroidIn
  , pattern Out
  , pattern CentroidOut
  , pattern Uniform
  , pattern Smooth
  , pattern Flat
  , pattern Noperspective
  , pattern Object_
  , pattern In_
  , pattern CentroidIn_
  , pattern Out_
  , pattern CentroidOut_
) where


-- ~
import Core
import qualified LLIR.SPMD.Internal.AST as IR
-- ~


-- | Top-Level Object Declaration
-- I.e. object initialization, or declaration.

pattern Object :: Maybe IR.StorageQualifier -> IR.Type -> IR.Ident -> IR.Object
pattern Object qualifier ty ident = IR.Object qualifier ty ident



-- | Storage Qualifiers
--

pattern Const :: IR.StorageQualifier
pattern Const = IR.ConstStorageQualifier

pattern In :: Maybe IR.InterpolationQualifier  -> IR.StorageQualifier
pattern In interpolation = IR.InStorageQualifier interpolation

pattern CentroidIn :: Maybe IR.InterpolationQualifier -> IR.StorageQualifier
pattern CentroidIn interpolation = IR.CentroidInStorageQualifier interpolation

pattern Out :: Maybe IR.InterpolationQualifier -> IR.StorageQualifier
pattern Out interpolation = IR.OutStorageQualifier interpolation

pattern CentroidOut :: Maybe IR.InterpolationQualifier -> IR.StorageQualifier
pattern CentroidOut interpolation = IR.CentroidOutStorageQualifier interpolation

pattern Uniform :: IR.StorageQualifier
pattern Uniform = IR.UniformStorageQualifier


-- | Interpolation Qualifiers
--

pattern Smooth :: IR.InterpolationQualifier
pattern Smooth = IR.SmoothInterpolation

pattern Flat :: IR.InterpolationQualifier
pattern Flat = IR.FlatInterpolation

pattern Noperspective :: IR.InterpolationQualifier
pattern Noperspective = IR.NoperspectiveInterpolation




-- | Alternate variations - convenience helpers
--


pattern Object_ :: IR.Type -> IR.Ident -> IR.Object
pattern Object_ ty ident <- IR.Object _ ty ident
    where
        Object_ ty ident = IR.Object Nothing ty ident



pattern In_ :: IR.StorageQualifier
pattern In_ <- IR.InStorageQualifier _
    where
        In_ = IR.InStorageQualifier Nothing

pattern CentroidIn_ :: IR.StorageQualifier
pattern CentroidIn_ <- IR.CentroidInStorageQualifier _
    where
        CentroidIn_ = IR.CentroidInStorageQualifier Nothing

pattern Out_ :: IR.StorageQualifier
pattern Out_ <- IR.OutStorageQualifier _
    where
        Out_ = IR.OutStorageQualifier Nothing

pattern CentroidOut_ :: IR.StorageQualifier
pattern CentroidOut_ <- IR.CentroidOutStorageQualifier _
    where
        CentroidOut_ = IR.CentroidOutStorageQualifier Nothing







