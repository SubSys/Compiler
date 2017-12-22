{-# LANGUAGE NoImplicitPrelude #-}
module SLIR.HelmSyntax.Feed.HelmIntro.TermLevel.Expressions (
    dropExpr
) where


-- *
import Core

import Prelude (return, String, IO, show, error, (<$>))
import qualified Text.Show.Prettyprint       as PP

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
import qualified SLIR.HelmSyntax.Feed.HelmIntro.TermLevel.Patterns as P
-- *



dropExpr :: (S.Decl.Function -> I.Decl.Function) -> S.E.Expr -> I.E.Expr

dropExpr f (S.E.Var id' _) =
    I.E.Var $ ID.toRef id'

dropExpr f (S.E.Lit val _) =
    I.E.Lit $ V.dropValue val

-- dropExpr f (S.E.Record fields _) =
--     I.E.Record $ map field fields
--     where
--         field (n, e) =
--             (ID.dropLow n, dropExpr f e)

dropExpr f (S.E.Tuple items _) =
    I.E.Tuple $ map (dropExpr f) items

dropExpr f (S.E.List xs _) =
    I.E.List $ map (dropExpr f) xs

dropExpr f (S.E.Con id' _) =
    I.E.Con $ ID.dropBig id'

dropExpr f (S.E.Let fns expr _) =
    I.E.Let
        (map f fns)
        (dropExpr f expr)


dropExpr f (S.E.Case expr alts _) =
    I.E.Case
        (dropExpr f expr)
        (map dropCaseAlt alts)

    where
        dropCaseAlt = P.dropCaseAlt (dropExpr f)

dropExpr f (S.E.Parens expr _) =
    dropExpr f expr

dropExpr f (S.E.App e1 e2 _) =
    I.E.App
        (dropExpr f e1)
        (dropExpr f e2)


dropExpr f (S.E.BinOp (S.ID.Sym txt ns _) e1 e2 _) =
    I.E.BinApp
        (I.ID.Ref txt (ID.dropNamespace ns))
        (dropExpr f e1)
        (dropExpr f e2)

dropExpr f (S.E.Abs arg expr _) =
    I.E.Abs
        (ID.toBinder arg)
        (dropExpr f expr)


dropExpr f (S.E.If intros elseExpr _) =
    I.E.If
        (map branch intros)
        (dropExpr f elseExpr)

    where
        branch (con, body) =
            (dropExpr f con, dropExpr f body)



