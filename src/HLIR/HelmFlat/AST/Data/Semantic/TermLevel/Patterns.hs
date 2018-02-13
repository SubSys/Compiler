{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module HLIR.HelmFlat.AST.Data.Semantic.TermLevel.Patterns (
    IR.Pattern
  , IR.CaseAlt(..)

  , pattern Lit
  , pattern List
  , pattern ListCons
  , pattern Tuple
  , pattern Constr
  , pattern Var
  , pattern Wildcard
) where


-- *
import Core

--- Local
import qualified HLIR.HelmFlat.Internal.AST as IR
-- *






pattern Lit ::  IR.LiteralValue -> IR.Pattern
pattern Lit lit = IR.LitPattern lit

pattern List :: [IR.Pattern] -> IR.Pattern
pattern List xs = IR.ListPattern xs

pattern ListCons :: [IR.Pattern] -> Maybe IR.Pattern -> IR.Pattern
pattern ListCons xs rest = IR.ListConsPattern xs rest

pattern Tuple :: [IR.Pattern] -> IR.Pattern
pattern Tuple items = IR.TuplePattern items

pattern Constr :: IR.Ident -> [IR.Pattern] -> IR.Pattern
pattern Constr ident args = IR.ConstrPattern ident args


pattern Var :: IR.Binder -> IR.Pattern
pattern Var ident = IR.VarPattern ident

pattern Wildcard :: IR.Pattern
pattern Wildcard = IR.WildcardPattern





