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
    
  , pattern Lit'
  , pattern List'
  , pattern ListCons'
  , pattern Tuple'
  , pattern Constr'
  , pattern Var'
  , pattern Wildcard'
) where


-- *
import Core

--- Local
import qualified HLIR.HelmFlat.Internal.AST as IR
-- *






pattern Lit ::  IR.LiteralValue -> IR.Meta -> IR.Pattern
pattern Lit lit meta = IR.LitPattern lit meta

pattern List :: [IR.Pattern] -> IR.Meta -> IR.Pattern
pattern List xs meta = IR.ListPattern xs meta

pattern ListCons :: [IR.Pattern] -> Maybe IR.Pattern -> IR.Meta -> IR.Pattern
pattern ListCons xs rest meta = IR.ListConsPattern xs rest meta

pattern Tuple :: [IR.Pattern] -> IR.Meta -> IR.Pattern
pattern Tuple items meta = IR.TuplePattern items meta

pattern Constr :: IR.Ident -> [IR.Pattern] -> IR.Meta -> IR.Pattern
pattern Constr ident args meta = IR.ConstrPattern ident args meta


pattern Var :: IR.Binder -> IR.Meta -> IR.Pattern
pattern Var ident meta = IR.VarPattern ident meta

pattern Wildcard :: IR.Meta -> IR.Pattern
pattern Wildcard meta = IR.WildcardPattern meta



-- | Alternative variations, or helpers.
--

pattern Lit' ::  IR.LiteralValue -> IR.Pattern
pattern Lit' lit <- IR.LitPattern lit _
    where
        Lit' lit = IR.LitPattern lit IR.Empty

pattern List' :: [IR.Pattern] -> IR.Pattern
pattern List' xs <- IR.ListPattern xs _
    where
        List' xs = IR.ListPattern xs IR.Empty

pattern ListCons' :: [IR.Pattern] -> Maybe IR.Pattern -> IR.Pattern
pattern ListCons' xs rest <- IR.ListConsPattern xs rest _
    where
        ListCons' xs rest = IR.ListConsPattern xs rest IR.Empty

pattern Tuple' :: [IR.Pattern] -> IR.Pattern
pattern Tuple' items <- IR.TuplePattern items _
    where
        Tuple' items = IR.TuplePattern items IR.Empty

pattern Constr' :: IR.Ident -> [IR.Pattern] -> IR.Pattern
pattern Constr' ident args <- IR.ConstrPattern ident args _
    where
        Constr' ident args = IR.ConstrPattern ident args IR.Empty

pattern Var' :: IR.Binder -> IR.Pattern
pattern Var' ident <- IR.VarPattern ident _
    where
        Var' ident = IR.VarPattern ident IR.Empty

pattern Wildcard' :: IR.Pattern
pattern Wildcard' <- IR.WildcardPattern _
    where
        Wildcard' = IR.WildcardPattern IR.Empty





