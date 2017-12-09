{-# LANGUAGE NoImplicitPrelude #-}
module HLIR.HelmOutro.Feed.LightRoast.TermLevel.Patterns (
    dropCaseAlt
) where


-- *
import Core

--- Local Deps
-- ~ HelmOutro AST
-- ~~ Base
import qualified HLIR.HelmOutro.AST.Base.Ident  as O.ID
import qualified HLIR.HelmOutro.AST.Base.Types  as O.T
import qualified HLIR.HelmOutro.AST.Base.Values as O.V
import qualified HLIR.HelmOutro.AST.Base.Etc    as O.Etc
-- ~~ TermLevel
import qualified HLIR.HelmOutro.AST.TermLevel.Expressions as O.E
import qualified HLIR.HelmOutro.AST.TermLevel.Patterns    as O.P
-- ~~ TopLevel
import qualified HLIR.HelmOutro.AST.TopLevel.Functions as O.Decl
import qualified HLIR.HelmOutro.AST.TopLevel.Unions    as O.Decl


-- ~ LightRoast AST
-- ~~ Base
import qualified LLIR.LightRoast.AST.Base.Etc           as L.Etc
import qualified LLIR.LightRoast.AST.Base.Ident         as L.ID
import qualified LLIR.LightRoast.AST.Base.Types         as L.T
import qualified LLIR.LightRoast.AST.Base.Values        as L.V
-- ~~ TermLevel
import qualified LLIR.LightRoast.AST.TermLevel.Block    as L.Decl
import qualified LLIR.LightRoast.AST.TermLevel.Patterns as L.P
import qualified LLIR.LightRoast.AST.TermLevel.Stmt     as L.S
-- ~~ TopLevel
import qualified LLIR.LightRoast.AST.TopLevel.Functions as L.Decl
import qualified LLIR.LightRoast.AST.TopLevel.Unions    as L.Decl

--- Local
import qualified HLIR.HelmOutro.Feed.LightRoast.Base.Ident  as ID
import qualified HLIR.HelmOutro.Feed.LightRoast.Base.Values as V
-- *




dropCaseAlt :: (O.E.Expr -> L.Decl.Block) -> O.P.CaseAlt -> L.P.CaseAlt
dropCaseAlt dropExpr (O.P.CaseAlt p e) =
    L.P.CaseAlt
        (dropPattern p)
        (dropExpr e)


dropPattern :: O.P.Pattern -> L.P.Pattern
dropPattern (O.P.Lit val) =
    L.P.Lit $ V.dropValue val

dropPattern (O.P.Record ids) =
    L.P.Record $ map ID.dropLow ids

dropPattern (O.P.List xs) =
    L.P.List $ map dropPattern xs

dropPattern (O.P.Cons xs rest) =
    L.P.Cons
        (map dropPattern xs)
        (dropRest rest)
        
    where
        dropRest Nothing = Nothing
        dropRest (Just p) = Just $ dropPattern p

dropPattern (O.P.Tuple items) =
    L.P.Tuple $ map dropPattern items

dropPattern (O.P.Con name args) =
    L.P.Con
        (ID.dropBig name)
        (map dropPattern args)

dropPattern (O.P.Var name) =
    L.P.Var $ ID.dropBinder name

dropPattern O.P.Wildcard =
    L.P.Wildcard



