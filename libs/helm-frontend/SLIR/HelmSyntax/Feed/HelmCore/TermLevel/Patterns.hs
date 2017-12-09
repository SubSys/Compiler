{-# LANGUAGE NoImplicitPrelude #-}
module SLIR.HelmSyntax.Feed.HelmCore.TermLevel.Patterns (
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

-- ~ HelmCore AST
-- ~~ Base
import qualified HLIR.HelmCore.AST.Base.Ident  as C.ID
import qualified HLIR.HelmCore.AST.Base.Types  as C.T
import qualified HLIR.HelmCore.AST.Base.Values as C.V
-- ~~ TermLevel
import qualified HLIR.HelmCore.AST.TermLevel.Expressions as C.E
import qualified HLIR.HelmCore.AST.TermLevel.Patterns    as C.P
-- ~~ TopLevel
import qualified HLIR.HelmCore.AST.TopLevel.Functions as C.Decl
import qualified HLIR.HelmCore.AST.TopLevel.Unions    as C.Decl

--- Local
import qualified SLIR.HelmSyntax.Feed.HelmCore.Base.Ident  as ID
import qualified SLIR.HelmSyntax.Feed.HelmCore.Base.Types  as T
import qualified SLIR.HelmSyntax.Feed.HelmCore.Base.Values as V
-- *


dropCaseAlt :: (S.E.Expr -> C.E.Expr) -> S.P.CaseAlt -> C.P.CaseAlt
dropCaseAlt dropExpr (S.P.CaseAlt p e _) =
    C.P.CaseAlt
        (dropPattern p)
        (dropExpr e)

dropPattern :: S.P.Pattern -> C.P.Pattern
dropPattern (S.P.Lit val _) =
    C.P.Lit $ V.dropValue val

dropPattern (S.P.Record ids _) =
    C.P.Record $ map ID.dropLow ids

dropPattern (S.P.List xs _) =
    C.P.List $ map dropPattern xs

dropPattern (S.P.Cons xs rest _) =
    C.P.Cons
        (map dropPattern xs)
        (dropRest rest)
        
    where
        dropRest Nothing = Nothing
        dropRest (Just p) = Just $ dropPattern p

dropPattern (S.P.Tuple items _) =
    C.P.Tuple $ map dropPattern items

dropPattern (S.P.Con name args _) =
    C.P.Con
        (ID.dropBig name)
        (map dropPattern args)

dropPattern (S.P.Var name _) =
    C.P.Var $ ID.toBinder name

dropPattern (S.P.Wildcard _) =
    C.P.Wildcard




