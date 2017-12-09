{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module HLIR.HelmCore.AST.TermLevel.Expressions (
      IR.Expr
    , pattern Var
    , pattern Lit
    , pattern Record
    , pattern Tuple
    , pattern List
    , pattern Con
    , pattern Let
    , pattern Case
    , pattern App
    , pattern Abs
) where


-- *
import Core

--- Local
import qualified HLIR.HelmCore.Internal.AST as IR
-- *



pattern Var :: IR.Ref ->  IR.Expr
pattern Var ref  = IR.VarExpr ref

pattern Lit ::  IR.LiteralValue ->  IR.Expr
pattern Lit val  = IR.LitExpr val 

pattern Record :: [(IR.Low, IR.Expr)] ->  IR.Expr
pattern Record fields  = IR.RecordExpr fields 

pattern Tuple :: [IR.Expr] ->  IR.Expr
pattern Tuple items  = IR.TupleExpr items 

pattern List :: [IR.Expr] ->  IR.Expr
pattern List xs  = IR.ListExpr xs 


-- |
-- A data constructor
--
pattern Con :: IR.Big ->  IR.Expr
pattern Con id'  = IR.ConExpr id' 


-- |
-- A let binding
--
pattern Let :: [IR.Function] -> IR.Expr ->  IR.Expr
pattern Let fns expr  = IR.LetExpr fns expr


-- |
-- A case expression.
--
pattern Case :: IR.Expr -> [IR.CaseAlt] ->  IR.Expr
pattern Case con alts  = IR.CaseExpr con alts 

pattern App :: IR.Expr -> IR.Expr ->  IR.Expr
pattern App e1 e2  = IR.AppExpr e1 e2 


pattern Abs :: IR.Binder -> IR.Expr ->  IR.Expr
pattern Abs arg expr  = IR.AbsExpr arg expr



