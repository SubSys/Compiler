{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module HLIR.HelmIntro.AST.Data.TermLevel.Patterns (
      IR.Pattern
    , IR.CaseAlt(..)

    , pattern Lit
    -- , pattern Record
    , pattern List
    , pattern Cons
    , pattern Tuple
    , pattern Con
    , pattern Var
    , pattern Wildcard
) where


-- *
import Core

--- Local
import qualified HLIR.HelmIntro.Internal.AST as IR
-- *




pattern Lit ::  IR.LiteralValue ->  IR.Pattern
pattern Lit lit = IR.LitPattern lit

-- pattern Record :: [IR.Low] ->  IR.Pattern
-- pattern Record vars = IR.RecordPattern vars

pattern List :: [IR.Pattern] ->  IR.Pattern
pattern List xs = IR.ListPattern xs

pattern Cons :: [IR.Pattern] -> Maybe IR.Pattern ->  IR.Pattern
pattern Cons xs rest = IR.ConsPattern xs rest

pattern Tuple :: [IR.Pattern] ->  IR.Pattern
pattern Tuple items = IR.TuplePattern items

pattern Con :: IR.Big -> [IR.Pattern] ->  IR.Pattern
pattern Con id' args = IR.ConPattern id' args


pattern Var :: IR.Binder ->  IR.Pattern
pattern Var id' = IR.VarPattern id'

pattern Wildcard ::  IR.Pattern
pattern Wildcard = IR.WildcardPattern

