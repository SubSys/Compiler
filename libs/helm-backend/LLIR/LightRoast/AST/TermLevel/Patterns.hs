{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module LLIR.LightRoast.AST.TermLevel.Patterns (
      IR.Pattern
    , IR.CaseAlt

    , pattern CaseAlt
    , pattern Lit
    , pattern Record
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
import qualified LLIR.LightRoast.Internal.AST as IR
-- *



pattern CaseAlt :: IR.Pattern -> IR.Block -> IR.CaseAlt
pattern CaseAlt patrn block = IR.CaseAlt patrn block


pattern Lit ::  IR.LiteralValue -> IR.Pattern
pattern Lit val = IR.LitPattern val

pattern Record :: [IR.Low] -> IR.Pattern
pattern Record xs = IR.RecordPattern xs

pattern List :: [IR.Pattern] -> IR.Pattern
pattern List xs = IR.ListPattern xs

pattern Cons :: [IR.Pattern] -> Maybe IR.Pattern -> IR.Pattern
pattern Cons xs end = IR.ConsPattern xs end

pattern Tuple :: [IR.Pattern] -> IR.Pattern
pattern Tuple items = IR.TuplePattern items

pattern Con :: IR.Big -> [IR.Pattern] -> IR.Pattern
pattern Con name args = IR.ConPattern name args

pattern Var :: IR.Low -> IR.Pattern
pattern Var ref = IR.VarPattern ref

pattern Wildcard :: IR.Pattern
pattern Wildcard = IR.WildcardPattern





