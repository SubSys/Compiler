{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
-- | Top-Level Type Declarations 
--
module CGIR.Rust.AST.TopLevel.TypeSystem (
      pattern Enum
    , pattern Struct
    , pattern StructVariant
    , pattern TupleVariant
    , pattern UnitVariant
    , IR.EnumDecl
    , IR.StructDecl
    , IR.VariantDecl
) where


-- *
import Core
import Data.Data (Data, Typeable)

--- Local
import qualified CGIR.Rust.Internal.AST        as IR
-- *



-- *
-- | Top-Level Declarations
-- *
pattern Enum :: IR.Big -> GenericArgs -> [IR.VariantDecl] -> IR.EnumDecl
pattern Enum name args variants = IR.EnumDecl name args variants

pattern Struct :: IR.Big -> [(IR.Low, IR.Type)] -> IR.StructDecl
pattern Struct name fields = IR.StructDecl name fields



-- *
-- | Enum Body Constructors (Variants)
-- *
pattern StructVariant :: IR.Big -> [(IR.Low, IR.Type)] -> IR.VariantDecl
pattern StructVariant name fields = IR.StructVariant name fields


pattern TupleVariant ::  IR.Big -> [IR.Type] -> IR.VariantDecl
pattern TupleVariant name xs = IR.TupleVariant name xs


pattern UnitVariant :: IR.Big -> IR.VariantDecl
pattern UnitVariant name = IR.UnitVariant name




-- *
-- | Struct Body Constructors
-- *






-- *
-- | Misc.
-- *
type GenericArgs = [IR.Big]


