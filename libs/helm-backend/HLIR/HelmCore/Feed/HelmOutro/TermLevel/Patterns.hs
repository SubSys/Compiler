{-# LANGUAGE NoImplicitPrelude #-}
module HLIR.HelmCore.Feed.HelmOutro.TermLevel.Patterns (
    dropCaseAlt
) where


-- *
import Core

--- Local Deps
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

-- ~ HelmOutro AST
-- ~ HelmCore AST
-- ~~ Base
import qualified HLIR.HelmOutro.AST.Base.Ident  as O.ID
import qualified HLIR.HelmOutro.AST.Base.Types  as O.T
import qualified HLIR.HelmOutro.AST.Base.Values as O.V
-- ~~ TermLevel
import qualified HLIR.HelmOutro.AST.TermLevel.Expressions as O.E
import qualified HLIR.HelmOutro.AST.TermLevel.Patterns    as O.P
-- ~~ TopLevel
import qualified HLIR.HelmOutro.AST.TopLevel.Functions as O.Decl
import qualified HLIR.HelmOutro.AST.TopLevel.Unions    as O.Decl


--- Local
import qualified HLIR.HelmCore.Feed.HelmOutro.Base.Ident  as ID
import qualified HLIR.HelmCore.Feed.HelmOutro.Base.Values as V
import qualified HLIR.HelmCore.Feed.HelmOutro.Base.Types  as T
-- *



dropCaseAlt :: (C.E.Expr -> O.E.Expr) -> C.P.CaseAlt -> O.P.CaseAlt
dropCaseAlt dropExpr (C.P.CaseAlt p e) =
    O.P.CaseAlt
        (dropPattern p)
        (dropExpr e)


dropPattern :: C.P.Pattern -> O.P.Pattern
dropPattern (C.P.Lit val) =
    O.P.Lit $ V.dropValue val

dropPattern (C.P.Record ids) =
    O.P.Record $ map ID.dropLow ids

dropPattern (C.P.List xs) =
    O.P.List $ map dropPattern xs

dropPattern (C.P.Cons xs rest) =
    O.P.Cons
        (map dropPattern xs)
        (dropRest rest)
        
    where
        dropRest Nothing = Nothing
        dropRest (Just p) = Just $ dropPattern p

dropPattern (C.P.Tuple items) =
    O.P.Tuple $ map dropPattern items

dropPattern (C.P.Con name args) =
    O.P.Con
        (ID.dropBig name)
        (map dropPattern args)

dropPattern (C.P.Var name) =
    O.P.Var $ ID.dropBinder name

dropPattern C.P.Wildcard =
    O.P.Wildcard



