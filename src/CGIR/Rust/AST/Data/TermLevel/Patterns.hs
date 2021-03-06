{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module CGIR.Rust.AST.Data.TermLevel.Patterns (
    IR.Pattern
  , IR.Arm
  
  , pattern Arm
  
  , pattern Var
  , pattern Lit
  , pattern List
  , pattern ListCons
  , pattern Tuple
  , pattern Variant
  , pattern Wildcard
) where


-- *
import Core

--- Local
import qualified CGIR.Rust.Internal.AST as IR
-- *


pattern Arm :: IR.Pattern -> IR.Block -> IR.Arm
pattern Arm patrn block = IR.Arm patrn block


-- | Pattern Items
--
pattern Var :: IR.Ident -> IR.Pattern
pattern Var ident = IR.VarPattern ident

pattern Lit :: IR.LiteralValue -> IR.Pattern
pattern Lit val = IR.LitPattern val

pattern List :: [IR.Pattern] -> IR.Pattern
pattern List xs = IR.ListPattern xs

pattern ListCons :: [IR.Pattern] -> Maybe IR.Pattern -> IR.Pattern
pattern ListCons xs end = IR.ListConsPattern xs end

pattern Tuple :: [IR.Pattern] -> IR.Pattern
pattern Tuple items = IR.TuplePattern items

pattern Variant :: IR.Path -> [IR.Pattern] -> IR.Pattern
pattern Variant path args = IR.VariantPattern path args

pattern Wildcard :: IR.Pattern
pattern Wildcard = IR.WildcardPattern



