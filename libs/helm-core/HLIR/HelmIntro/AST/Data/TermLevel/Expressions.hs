{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module HLIR.HelmIntro.AST.Data.TermLevel.Expressions (
      IR.Expr
    , pattern Var
    , pattern Lit
    , pattern Tuple
    , pattern List
    , pattern Con
    , pattern If
    , pattern Let
    , pattern Case
    , pattern Parens
    , pattern App
    , pattern BinApp
    , pattern Abs
) where


-- *
import Core
import Prelude (show)

import qualified Data.Text as Text


--- Local
import qualified HLIR.HelmIntro.Internal.AST as IR
-- *







pattern Var :: IR.Ref ->  IR.Expr
pattern Var name = IR.VarExpr name


pattern Lit ::  IR.LiteralValue ->  IR.Expr
pattern Lit val = IR.LitExpr val

pattern Tuple :: [IR.Expr] ->  IR.Expr
pattern Tuple items = IR.TupleExpr items

pattern List :: [IR.Expr] ->  IR.Expr
pattern List xs = IR.ListExpr xs


-- |
-- A data constructor
--
pattern Con :: IR.Big ->  IR.Expr
pattern Con id' = IR.ConExpr id'


pattern If :: [(IR.Expr, IR.Expr)] -> IR.Expr ->  IR.Expr
pattern If intros elseExpr = IR.IfExpr intros elseExpr



-- |
-- A let binding
--
pattern Let :: [IR.Function] -> IR.Expr ->  IR.Expr
pattern Let fns expr = IR.LetExpr fns expr


-- |
-- A case expression.
--
pattern Case :: IR.Expr -> [IR.CaseAlt] ->  IR.Expr
pattern Case con alts = IR.CaseExpr con alts



pattern Parens :: IR.Expr ->  IR.Expr
pattern Parens expr = IR.ParensExpr expr


pattern App :: IR.Expr -> IR.Expr ->  IR.Expr
pattern App e1 e2 = IR.AppExpr e1 e2

pattern BinApp :: IR.Ref -> IR.Expr -> IR.Expr -> IR.Expr
pattern BinApp name e1 e2 = IR.BinAppExpr name e1 e2

pattern Abs :: IR.Binder -> IR.Expr ->  IR.Expr
pattern Abs arg expr = IR.AbsExpr arg expr




