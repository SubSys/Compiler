{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module LLIR.HelmLL.AST.Data.TermLevel.Patterns (
    IR.CaseAlt
  , IR.Pattern

  , pattern CaseAlt
  , pattern Var
  , pattern Lit
  , pattern List
  , pattern ListCons
  , pattern Tuple
  , pattern Constr
  , pattern Wildcard

  , pattern CaseAlt_
) where


-- ~
import Core
import Core.Utils as Core
import Core.List.Util (singleton)

import Prelude (error, ($))

import qualified Data.List    as List
import qualified Data.Maybe   as Maybe
import qualified Data.Text    as Text
import qualified Data.String  as String

import qualified LLIR.HelmLL.Internal.AST as IR
-- ~



pattern CaseAlt :: IR.Pattern -> IR.Block -> IR.CaseAlt
pattern CaseAlt patrn block = IR.CaseAlt patrn block



-- | Pattern Nodes
--

pattern Var :: IR.Binder -> IR.Pattern
pattern Var name = IR.VarPattern name

pattern Lit :: IR.LiteralValue -> IR.Pattern
pattern Lit val = IR.LitPattern val

pattern List :: [IR.Pattern] -> IR.Pattern
pattern List xs = IR.ListPattern xs

pattern ListCons :: [IR.Pattern] -> Maybe IR.Pattern -> IR.Pattern
pattern ListCons xs rest = IR.ListConsPattern xs rest

pattern Tuple :: [IR.Pattern] -> IR.Pattern
pattern Tuple items = IR.TuplePattern items

pattern Constr :: IR.Ident -> [IR.Pattern] -> IR.Pattern
pattern Constr constr args = IR.ConstrPattern constr args

pattern Wildcard :: IR.Pattern
pattern Wildcard = IR.WildcardPattern



-- | Alternate variations - convenience helpers
--

pattern CaseAlt_ :: IR.Pattern -> [IR.Stmt] -> IR.CaseAlt
pattern CaseAlt_ patrn stmts = IR.CaseAlt patrn (IR.Block stmts)


