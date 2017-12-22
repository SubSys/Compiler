{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module SLIR.HelmSyntax.AST.Data.TermLevel.Expressions (
      IR.Expr
    , pattern Var
    , pattern Lit
    , pattern Record
    , pattern Tuple
    , pattern List
    , pattern Con
    , pattern BinOp
    , pattern If
    , pattern Let
    , pattern Case
    , pattern RecordUpdate
    , pattern RecordAccess
    , pattern Parens
    , pattern App
    , pattern Abs
    
    , pattern Var'
    , pattern Lit'
    , pattern Record'
    , pattern Tuple'
    , pattern List'
    , pattern Con'
    , pattern BinOp'
    , pattern If'
    , pattern Let'
    , pattern Case'
    , pattern RecordUpdate'
    , pattern RecordAccess'
    , pattern Parens'
    , pattern App'
    , pattern Abs'
    
    -- Specials
    , pattern AltAbs
) where


-- *
import Core
import Prelude (show)

import qualified Data.Text as Text


--- Local
import qualified SLIR.HelmSyntax.Internal.AST as IR

-- ~ AST - Essential Instances
import SLIR.HelmSyntax.Internal.AST.Instances.Essential ()
-- *







pattern Var :: IR.Low -> Maybe IR.Meta -> IR.Expr
pattern Var name metaOpt = IR.VarExpr name metaOpt


pattern Lit ::  IR.LiteralValue -> Maybe IR.Meta -> IR.Expr
pattern Lit val metaOpt = IR.LitExpr val metaOpt

pattern Record :: [(IR.Low, IR.Expr)] -> Maybe IR.Meta -> IR.Expr
pattern Record fields metaOpt = IR.RecordExpr fields metaOpt

pattern Tuple :: [IR.Expr] -> Maybe IR.Meta -> IR.Expr
pattern Tuple items metaOpt = IR.TupleExpr items metaOpt

pattern List :: [IR.Expr] -> Maybe IR.Meta -> IR.Expr
pattern List xs metaOpt = IR.ListExpr xs metaOpt


-- |
-- A data constructor
--
pattern Con :: IR.Big -> Maybe IR.Meta -> IR.Expr
pattern Con id' metaOpt = IR.ConExpr id' metaOpt

pattern BinOp :: IR.Sym -> IR.Expr -> IR.Expr -> Maybe IR.Meta -> IR.Expr
pattern BinOp sym e1 e2 metaOpt = IR.BinOpExpr sym e1 e2 metaOpt


pattern If :: [(IR.Expr, IR.Expr)] -> IR.Expr -> Maybe IR.Meta -> IR.Expr
pattern If intros elseExpr metaOpt = IR.IfExpr intros elseExpr metaOpt



-- |
-- A let binding
--
pattern Let :: [IR.Function] -> IR.Expr -> Maybe IR.Meta -> IR.Expr
pattern Let fns expr metaOpt = IR.LetExpr fns expr metaOpt


-- |
-- A case expression.
--
pattern Case :: IR.Expr -> [IR.CaseAlt] -> Maybe IR.Meta -> IR.Expr
pattern Case con alts metaOpt = IR.CaseExpr con alts metaOpt

pattern RecordUpdate :: IR.Low -> [(IR.Low, IR.Expr)] -> Maybe IR.Meta -> IR.Expr
pattern RecordUpdate var fields metaOpt = IR.RecordUpdateExpr var fields metaOpt

-- |
-- A `.<tag>` that is either anonymous,
-- or associated with an explicit record object.
--
pattern RecordAccess :: IR.Low -> Maybe IR.Expr -> Maybe IR.Meta -> IR.Expr
pattern RecordAccess field object metaOpt = IR.RecordAccessExpr field object metaOpt


pattern Parens :: IR.Expr -> Maybe IR.Meta -> IR.Expr
pattern Parens expr metaOpt = IR.ParensExpr expr metaOpt


pattern App :: IR.Expr -> IR.Expr -> Maybe IR.Meta -> IR.Expr
pattern App e1 e2 metaOpt = IR.AppExpr e1 e2 metaOpt


pattern Abs :: IR.Low -> IR.Expr -> Maybe IR.Meta -> IR.Expr
pattern Abs arg expr metaOpt = IR.AbsExpr arg expr metaOpt



-- *
-- | Misc.
-- *

pattern Var' :: IR.Low -> IR.Expr
pattern Var' id' = IR.VarExpr id' Nothing

pattern Lit' ::  IR.LiteralValue -> IR.Expr
pattern Lit' val = IR.LitExpr val Nothing

pattern Record' :: [(IR.Low, IR.Expr)] -> IR.Expr
pattern Record' fields = IR.RecordExpr fields Nothing

pattern Tuple' :: [IR.Expr] -> IR.Expr
pattern Tuple' items = IR.TupleExpr items Nothing

pattern List' :: [IR.Expr] -> IR.Expr
pattern List' xs = IR.ListExpr xs Nothing


pattern Con' :: IR.Big -> IR.Expr
pattern Con' id' = IR.ConExpr id' Nothing

pattern BinOp' :: IR.Sym -> IR.Expr -> IR.Expr -> IR.Expr
pattern BinOp' sym e1 e2 = IR.BinOpExpr sym e1 e2 Nothing

pattern If' :: [(IR.Expr, IR.Expr)] -> IR.Expr -> IR.Expr
pattern If' intros elseExpr = IR.IfExpr intros elseExpr Nothing


pattern Let' :: [IR.Function] -> IR.Expr -> IR.Expr
pattern Let' fns expr = IR.LetExpr fns expr Nothing

pattern RecordAccess' :: IR.Low -> Maybe IR.Expr -> IR.Expr
pattern RecordAccess' field object = IR.RecordAccessExpr field object Nothing

pattern Parens' :: IR.Expr -> IR.Expr
pattern Parens' expr = IR.ParensExpr expr Nothing


pattern App' :: IR.Expr -> IR.Expr -> IR.Expr
pattern App' e1 e2 = IR.AppExpr e1 e2 Nothing

pattern Abs' :: IR.Low -> IR.Expr -> IR.Expr
pattern Abs' arg expr = IR.AbsExpr arg expr Nothing

pattern Case' :: IR.Expr -> [IR.CaseAlt] -> IR.Expr
pattern Case' con alts = IR.CaseExpr con alts Nothing

pattern RecordUpdate' :: IR.Low -> [(IR.Low, IR.Expr)] -> IR.Expr
pattern RecordUpdate' var fields = IR.RecordUpdateExpr var fields Nothing





-- *
-- | Specials
-- *


pattern AltAbs :: [IR.Low] -> IR.Expr -> Maybe IR.Scheme -> IR.Expr
pattern AltAbs args expr scheme = IR.AltAbsExpr args expr scheme



