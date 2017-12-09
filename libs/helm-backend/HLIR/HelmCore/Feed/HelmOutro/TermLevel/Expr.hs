{-# LANGUAGE NoImplicitPrelude #-}
module HLIR.HelmCore.Feed.HelmOutro.TermLevel.Expr where


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
import qualified HLIR.HelmCore.Feed.HelmOutro.Base.Ident         as ID
import qualified HLIR.HelmCore.Feed.HelmOutro.Base.Values        as V
import qualified HLIR.HelmCore.Feed.HelmOutro.Base.Types         as T
import qualified HLIR.HelmCore.Feed.HelmOutro.TermLevel.Patterns as P
import qualified HLIR.HelmCore.Feed.HelmOutro.Base.Etc           as Etc
-- *



dropExpr :: (C.Decl.Function -> O.Decl.Function) -> C.E.Expr -> O.E.Expr

dropExpr f (C.E.Var id') =
    O.E.Var $ ID.dropRef id'

dropExpr f (C.E.Lit val) =
    O.E.Lit $ V.dropValue val

dropExpr f (C.E.Record fields) =
    O.E.Record $ map field fields
    where
        field (n, e) =
            (ID.dropLow n, dropExpr f e)

dropExpr f (C.E.Tuple items) =
    O.E.Tuple $ map (dropExpr f) items

dropExpr f (C.E.List xs) =
    O.E.List $ map (dropExpr f) xs

dropExpr f (C.E.Con id') =
    O.E.Con $ ID.dropBig id'

dropExpr f (C.E.Let fns expr) =
    O.E.Let
        (map f fns)
        (dropExpr f expr)


dropExpr f (C.E.Case expr alts) =
    O.E.Case
        (dropExpr f expr)
        (map dropCaseAlt alts)

    where
        dropCaseAlt = P.dropCaseAlt (dropExpr f)

dropExpr f (C.E.App e1 e2) =
    O.E.App
        (dropExpr f e1)
        (dropExpr f e2)

dropExpr f (C.E.Abs arg expr) =
    O.E.Abs
        [Etc.toArg arg]
        (dropExpr f expr)



