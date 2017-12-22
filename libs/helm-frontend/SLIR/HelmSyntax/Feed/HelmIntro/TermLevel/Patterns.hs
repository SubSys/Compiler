{-# LANGUAGE NoImplicitPrelude #-}
module SLIR.HelmSyntax.Feed.HelmIntro.TermLevel.Patterns (
      dropCaseAlt
    , dropPattern
) where


-- *
import Core

--- Local Deps
-- ~ HelmSyntax AST
-- ~~ Base
import qualified SLIR.HelmSyntax.AST.Data.Base.Etc    as S.Etc
import qualified SLIR.HelmSyntax.AST.Data.Base.Ident  as S.ID
import qualified SLIR.HelmSyntax.AST.Data.Base.Types  as S.T
import qualified SLIR.HelmSyntax.AST.Data.Base.Values as S.V

-- ~~ TermLevel
import qualified SLIR.HelmSyntax.AST.Data.TermLevel.Expressions as S.E
import qualified SLIR.HelmSyntax.AST.Data.TermLevel.Patterns    as S.P

-- ~~ TopLevel
import qualified SLIR.HelmSyntax.AST.Data.TopLevel.Fixities  as S.Decl
import qualified SLIR.HelmSyntax.AST.Data.TopLevel.Functions as S.Decl
import qualified SLIR.HelmSyntax.AST.Data.TopLevel.Unions    as S.Decl

-- ~ HelmIntro AST
-- ~~ Base
import qualified HLIR.HelmIntro.AST.Data.Base.Ident  as I.ID
import qualified HLIR.HelmIntro.AST.Data.Base.Types  as I.T
import qualified HLIR.HelmIntro.AST.Data.Base.Values as I.V
-- ~~ TermLevel
import qualified HLIR.HelmIntro.AST.Data.TermLevel.Expressions as I.E
import qualified HLIR.HelmIntro.AST.Data.TermLevel.Patterns    as I.P
-- ~~ TopLevel
import qualified HLIR.HelmIntro.AST.Data.TopLevel.Functions as I.Decl
import qualified HLIR.HelmIntro.AST.Data.TopLevel.Unions    as I.Decl

--- Local
import qualified SLIR.HelmSyntax.Feed.HelmIntro.Base.Ident  as ID
import qualified SLIR.HelmSyntax.Feed.HelmIntro.Base.Types  as T
import qualified SLIR.HelmSyntax.Feed.HelmIntro.Base.Values as V
-- *


dropCaseAlt :: (S.E.Expr -> I.E.Expr) -> S.P.CaseAlt -> I.P.CaseAlt
dropCaseAlt dropExpr (S.P.CaseAlt p e _) =
    I.P.CaseAlt
        (dropPattern p)
        (dropExpr e)

dropPattern :: S.P.Pattern -> I.P.Pattern
dropPattern (S.P.Lit val _) =
    I.P.Lit $ V.dropValue val

-- dropPattern (S.P.Record ids _) =
--     I.P.Record $ map ID.dropLow ids

dropPattern (S.P.List xs _) =
    I.P.List $ map dropPattern xs

dropPattern (S.P.Cons xs rest _) =
    I.P.Cons
        (map dropPattern xs)
        (dropRest rest)
        
    where
        dropRest Nothing = Nothing
        dropRest (Just p) = Just $ dropPattern p

dropPattern (S.P.Tuple items _) =
    I.P.Tuple $ map dropPattern items

dropPattern (S.P.Con name args _) =
    I.P.Con
        (ID.dropBig name)
        (map dropPattern args)

dropPattern (S.P.Var name _) =
    I.P.Var $ ID.toBinder name

dropPattern (S.P.Wildcard _) =
    I.P.Wildcard




