{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module GCIR.RustCG.AST.Data.Semantic.BlockLevel.Patterns (
    IR.Pattern
  , pattern Var
  , pattern Lit
  , pattern List
  , pattern ListCons
  , pattern Tuple
  , pattern Constr
  , pattern Wildcard
) where


-- *
import Core

--- Local
import qualified GCIR.RustCG.Internal.AST as IR
-- *



pattern Var :: IR.Ident -> IR.Pattern
pattern Var ident = IR.VarPattern (IR.Binder ident)

pattern Lit :: IR.LiteralValue -> IR.Pattern
pattern Lit val = IR.LitPattern val

pattern List :: [IR.Pattern] -> IR.Pattern
pattern List xs = IR.ListPattern xs

pattern ListCons :: [IR.Pattern] -> Maybe IR.Pattern -> IR.Pattern
pattern ListCons xs end = IR.ListConsPattern xs end

pattern Tuple :: [IR.Pattern] -> IR.Pattern
pattern Tuple items = IR.TuplePattern items

pattern Constr :: IR.Path -> [IR.Pattern] -> IR.Pattern
pattern Constr path args = IR.ConstrPattern path args

pattern Wildcard :: IR.Pattern
pattern Wildcard = IR.WildcardPattern



