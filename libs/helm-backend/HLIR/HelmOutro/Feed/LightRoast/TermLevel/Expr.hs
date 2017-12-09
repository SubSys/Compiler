{-# LANGUAGE NoImplicitPrelude #-}
module HLIR.HelmOutro.Feed.LightRoast.TermLevel.Expr (
    dropExpr
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
import qualified HLIR.HelmOutro.Feed.LightRoast.Base.Ident         as ID
import qualified HLIR.HelmOutro.Feed.LightRoast.Base.Values        as V
import qualified HLIR.HelmOutro.Feed.LightRoast.TermLevel.Patterns as P
import qualified HLIR.HelmOutro.Feed.LightRoast.Base.Etc           as Etc
import qualified HLIR.HelmOutro.Feed.LightRoast.Base.Types         as T
-- *




dropExpr :: O.E.Expr -> L.S.Stmt

dropExpr (O.E.Var id') =
    L.S.Ref $ ID.dropRef id'

dropExpr (O.E.Lit val) =
    L.S.Lit $ V.dropValue val

dropExpr (O.E.Tuple items) =
    L.S.Tuple $ map dropExpr items

dropExpr (O.E.List xs) =
    L.S.List $ map dropExpr xs

dropExpr (O.E.Case expr alts) =
    L.S.Case
        (dropExpr expr)
        (map dropCaseAlt alts)

    where
        dropCaseAlt = P.dropCaseAlt (toBlock . dropExpr)
    
        toBlock s = L.Decl.Block [s]


dropExpr (O.E.FunCall ref args) =
    L.S.FunCall
        (ID.dropRef ref)
        (map dropExpr args)

dropExpr (O.E.ConCall con args) =
    L.S.ConCall
        (ID.dropBig con)
        (map dropExpr args)


-- TODO: Add...
-- dropExpr f (O.E.Record fields) =


-- TODO: Error Out With Message
-- * These nodes should have been removed, since they aren’t currently supported in LLIR-LightRoast…
--
-- dropExpr f O.E.Let{} =
-- dropExpr f O.E.Abs{} =
-- dropExpr f O.E.App{} =
-- dropExpr f O.E.Con{} =



