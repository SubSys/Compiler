{-# LANGUAGE NoImplicitPrelude #-}
module HLIR.HelmIntro.Feed.HelmCore.TermLevel.Expressions (
    dropExpr
) where


-- *
import Core

--- Local Deps
-- ~ HelmSyntax AST
-- ~~ Base
import qualified HLIR.HelmIntro.AST.Data.Base.Etc    as S.Etc
import qualified HLIR.HelmIntro.AST.Data.Base.Ident  as S.ID
import qualified HLIR.HelmIntro.AST.Data.Base.Types  as S.T
import qualified HLIR.HelmIntro.AST.Data.Base.Values as S.V

-- ~~ TermLevel
import qualified HLIR.HelmIntro.AST.Data.TermLevel.Expressions as S.E
import qualified HLIR.HelmIntro.AST.Data.TermLevel.Patterns    as S.P

-- ~~ TopLevel
import qualified HLIR.HelmIntro.AST.Data.TopLevel.Fixities  as S.Decl
import qualified HLIR.HelmIntro.AST.Data.TopLevel.Functions as S.Decl
import qualified HLIR.HelmIntro.AST.Data.TopLevel.Unions    as S.Decl

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
import qualified HLIR.HelmIntro.Feed.HelmCore.Base.Ident  as ID
import qualified HLIR.HelmIntro.Feed.HelmCore.Base.Types  as T
import qualified HLIR.HelmIntro.Feed.HelmCore.Base.Values as V
import qualified HLIR.HelmIntro.Feed.HelmCore.TermLevel.Patterns as P
-- *



dropExpr :: (S.Decl.Function -> C.Decl.Function) -> S.E.Expr -> C.E.Expr

dropExpr f (S.E.Var id' _) =
    C.E.Var $ ID.toRef id'

dropExpr f (S.E.Lit val _) =
    C.E.Lit $ V.dropValue val

dropExpr f (S.E.Record fields _) =
    C.E.Record $ map field fields
    where
        field (n, e) =
            (ID.dropLow n, dropExpr f e)

dropExpr f (S.E.Tuple items _) =
    C.E.Tuple $ map (dropExpr f) items

dropExpr f (S.E.List xs _) =
    C.E.List $ map (dropExpr f) xs

dropExpr f (S.E.Con id' _) =
    C.E.Con $ ID.dropBig id'

dropExpr f (S.E.Let fns expr _) =
    C.E.Let
        (map f fns)
        (dropExpr f expr)


dropExpr f (S.E.Case expr alts _) =
    C.E.Case
        (dropExpr f expr)
        (map dropCaseAlt alts)

    where
        dropCaseAlt = P.dropCaseAlt (dropExpr f)

dropExpr f (S.E.Parens expr _) =
    dropExpr f expr

dropExpr f (S.E.App e1 e2 _) =
    C.E.App
        (dropExpr f e1)
        (dropExpr f e2)

dropExpr f (S.E.Abs arg expr _) =
    C.E.Abs
        (ID.toBinder arg)
        (dropExpr f expr)





