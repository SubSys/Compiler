{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module CGIR.Rust.AST.BlockLevel.Patterns (
      pattern Arm
    , pattern Var
    , pattern Wildcard
    , pattern Scalar
    , pattern Variant
    , pattern Tuple
    , IR.Arm
    , IR.Pattern
    , PatternPath
) where


-- *
import Core
import Data.Data (Data, Typeable)

--- Local
import qualified CGIR.Rust.Internal.AST        as IR
-- *



-- *
-- | Pattern Branch
-- *
pattern Arm :: IR.Pattern -> IR.Stmt -> IR.Arm
pattern Arm binder stmt = IR.Arm binder stmt






-- *
-- | Pattrn Binders
-- *

pattern Var :: IR.Low -> IR.Pattern
pattern Var x = IR.VarPattern x


pattern Wildcard :: IR.Pattern
pattern Wildcard = IR.WildcardPattern



pattern Scalar :: IR.ScalarValue -> IR.Pattern
pattern Scalar value = IR.ScalarPattern value


pattern Variant :: PatternPath  -> IR.Pattern
pattern Variant path = IR.VariantPattern path 


pattern Tuple :: [IR.Pattern] -> IR.Pattern
pattern Tuple args = IR.TuplePattern args



-- *
-- | Misc.
-- *
type PatternPath = IR.Path IR.Pattern




