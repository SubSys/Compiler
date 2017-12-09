{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module SLIR.HelmSyntax.AST.Data.TermLevel.Patterns (
      IR.Pattern
    , IR.CaseAlt(..)

    , pattern Lit
    , pattern Record
    , pattern List
    , pattern Cons
    , pattern Tuple
    , pattern Con
    , pattern Var
    , pattern Wildcard
    
    , pattern Lit'
    , pattern Record'
    , pattern List'
    , pattern Cons'
    , pattern Tuple'
    , pattern Con'
    , pattern Var'
    , pattern Wildcard'
) where


-- *
import Core

--- Local
import qualified SLIR.HelmSyntax.Internal.AST as IR

-- ~ AST - Essential Instances
import SLIR.HelmSyntax.Internal.AST.Instances.Essential ()
-- *




pattern Lit ::  IR.LiteralValue -> Maybe IR.Meta -> IR.Pattern
pattern Lit lit metaOpt = IR.LitPattern lit metaOpt

pattern Record :: [IR.Low Text] -> Maybe IR.Meta -> IR.Pattern
pattern Record vars metaOpt = IR.RecordPattern vars metaOpt

pattern List :: [IR.Pattern] -> Maybe IR.Meta -> IR.Pattern
pattern List xs metaOpt = IR.ListPattern xs metaOpt

pattern Cons :: [IR.Pattern] -> Maybe IR.Pattern -> Maybe IR.Meta -> IR.Pattern
pattern Cons xs rest metaOpt = IR.ConsPattern xs rest metaOpt

pattern Tuple :: [IR.Pattern] -> Maybe IR.Meta -> IR.Pattern
pattern Tuple items metaOpt = IR.TuplePattern items metaOpt

pattern Con :: IR.Big Text -> [IR.Pattern] -> Maybe IR.Meta -> IR.Pattern
pattern Con id' args metaOpt = IR.ConPattern id' args metaOpt


pattern Var :: IR.Low Text -> Maybe IR.Meta -> IR.Pattern
pattern Var id' metaOpt = IR.VarPattern id' metaOpt

pattern Wildcard :: Maybe IR.Meta -> IR.Pattern
pattern Wildcard metaOpt = IR.WildcardPattern metaOpt



-- *
-- | Misc.
-- *

pattern Lit' ::  IR.LiteralValue -> IR.Pattern
pattern Lit' lit = IR.LitPattern lit Nothing

pattern Record' :: [IR.Low Text] -> IR.Pattern
pattern Record' vars = IR.RecordPattern vars Nothing

pattern List' :: [IR.Pattern] -> IR.Pattern
pattern List' xs = IR.ListPattern xs Nothing

pattern Cons' :: [IR.Pattern] -> Maybe IR.Pattern -> IR.Pattern
pattern Cons' xs rest = IR.ConsPattern xs rest Nothing

pattern Tuple' :: [IR.Pattern] -> IR.Pattern
pattern Tuple' items = IR.TuplePattern items Nothing

pattern Con' :: IR.Big Text -> [IR.Pattern] -> IR.Pattern
pattern Con' id' args = IR.ConPattern id' args Nothing

-- pattern Var' :: IR.Low -> IR.Pattern
-- pattern Var' id' = IR.VarPattern (IR.Binder id' IR.BinderMarker) Nothing

pattern Var' :: IR.Low Text -> IR.Pattern
pattern Var' id' = IR.VarPattern id' Nothing

pattern Wildcard' :: IR.Pattern
pattern Wildcard' = IR.WildcardPattern Nothing



