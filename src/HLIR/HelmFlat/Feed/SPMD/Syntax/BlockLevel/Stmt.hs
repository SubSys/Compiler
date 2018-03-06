{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns #-}
module HLIR.HelmFlat.Feed.SPMD.Syntax.BlockLevel.Stmt (
    dropExpr
) where


-- *
import Core
import Core.Control.Flow ((|>), (<|))
import Core.List.Util    (flatten, singleton)
import Data.Monoid ((<>))
import Prelude
    ( return
    , String
    , IO
    , show
    , error
    , (<$>)
    , (>>=)
    , (>>)
    , fromIntegral
    )

import qualified Prelude    as Pre
import qualified Core.Utils as Core

import qualified Control.Monad              as M
import qualified Control.Monad.State        as M
import qualified Control.Monad.Except       as M
import qualified Control.Monad.RWS          as M
import qualified Control.Monad.Identity     as M
import qualified Control.Monad.Reader       as M
import qualified Control.Monad.Writer       as M
import qualified Control.Monad.Trans        as M

import qualified Data.List                    as List
import qualified Data.Text                    as Text
import qualified Data.Text.IO                 as TIO
import qualified Data.Map                     as Map
import qualified Data.Set                     as Set
import qualified Data.Foldable                as Fold
import qualified Data.Monoid                  as Monoid
import qualified Data.Maybe                   as Maybe
import qualified Data.Either                  as Either
import qualified Data.Char                    as Char
import qualified Data.Word                    as Word
import qualified Data.STRef                   as ST
import qualified Data.Bits                    as Bit
import qualified Data.Fixed                   as Fixed
import qualified Data.Vector.Unboxed          as V
import qualified Data.Vector.Unboxed.Mutable  as MV
import qualified Data.Vector.Generic          as VG
import qualified Data.IORef                   as IORef
import qualified Data.ByteString              as BS
import qualified Data.Functor                 as Fun
import qualified Data.String                  as String

-- + Recursion Schemes & Related
import qualified Data.Functor.Foldable       as F
import qualified Data.Generics.Uniplate.Data as Uni

-- + OS APIS & Related
import qualified System.IO as SIO

-- + Dev & Debugging
import qualified Text.Show.Prettyprint as PP


-- + HelmFlat AST Utils
import qualified HLIR.HelmFlat.AST.Utils.Types                    as Type
import qualified HLIR.HelmFlat.AST.Utils.Generic.SudoFFI          as SudoFFI
import qualified HLIR.HelmFlat.AST.Utils.Generic.TypesEnv         as TyEnv
import qualified HLIR.HelmFlat.AST.Utils.Generic.TypesEnv.Helpers as TyEnv

-- + HelmFlat AST
-- ++ Base
import qualified HLIR.HelmFlat.AST.Data.Semantic.Base.Etc           as H.Etc
import qualified HLIR.HelmFlat.AST.Data.Semantic.Base.Ident         as H.ID
import qualified HLIR.HelmFlat.AST.Data.Semantic.Base.Types         as H.T
import qualified HLIR.HelmFlat.AST.Data.Semantic.Base.Values        as H.V
-- ++ TermLevel
import qualified HLIR.HelmFlat.AST.Data.Semantic.TermLevel.Expr     as H.E
import qualified HLIR.HelmFlat.AST.Data.Semantic.TermLevel.Patterns as H.P
-- ++ TopLevel
import qualified HLIR.HelmFlat.AST.Data.Semantic.TopLevel.Functions as H.Decl
import qualified HLIR.HelmFlat.AST.Data.Semantic.TopLevel.Unions    as H.Decl

-- + SPMD AST
-- ++ Base
import qualified LLIR.SPMD.AST.Data.Base.Ident                 as S.ID
import qualified LLIR.SPMD.AST.Data.Base.Literals              as S.Lit
import qualified LLIR.SPMD.AST.Data.Base.Types                 as S.T
import qualified LLIR.SPMD.AST.Data.Base.Etc                   as S.Etc
-- ++ Block Level
import qualified LLIR.SPMD.AST.Data.BlockLevel.Stmt            as S.S
-- ++ Decl/Top Level
import qualified LLIR.SPMD.AST.Data.TopLevel.Functions         as S.Decl
import qualified LLIR.SPMD.AST.Data.TopLevel.Globals           as S.Decl

-- + Local
import qualified HLIR.HelmFlat.Feed.SPMD.Utils.Error as Error

import qualified HLIR.HelmFlat.Feed.SPMD.Syntax.Base.Etc      as Etc
import qualified HLIR.HelmFlat.Feed.SPMD.Syntax.Base.Ident    as ID
import qualified HLIR.HelmFlat.Feed.SPMD.Syntax.Base.Literals as Lit
import qualified HLIR.HelmFlat.Feed.SPMD.Syntax.Base.Types    as T
-- *




dropExpr :: H.E.Expr -> S.S.Stmt
dropExpr (builtin -> Just s) = s

dropExpr (H.E.Lit val) = S.S.Lit (Lit.dropLiteral val)
dropExpr (H.E.Ref ident) = S.S.Ref (ID.dropIdent ident)

dropExpr (H.E.FunCall name args) =
    S.S.FunCall
        (ID.dropIdent name)
        (map dropExpr args)

dropExpr (H.E.Case caseExpr (isBoolPatterns -> Just (trueExpr, falseExpr))) =
    S.S.If
        [ ( dropExpr caseExpr
          , S.S.Block
            [ dropExpr trueExpr
            ]
          )
        ]
        (Just $ S.S.Block [dropExpr falseExpr])





isBoolPatterns :: [H.P.CaseAlt] -> Maybe (H.E.Expr, H.E.Expr)
isBoolPatterns [isTrueBranch -> Just trueExpr, isFalseBranch -> Just falseExpr] = Just (trueExpr, falseExpr)
isBoolPatterns _ = Nothing


isTrueBranch :: H.P.CaseAlt -> Maybe H.E.Expr
isTrueBranch (H.P.CaseAlt (isBoolPatrn -> Just True) e) = Just e
isTrueBranch _ = Nothing

isFalseBranch :: H.P.CaseAlt -> Maybe H.E.Expr
isFalseBranch (H.P.CaseAlt (isBoolPatrn -> Just False) e) = Just e
isFalseBranch _ = Nothing

isBoolPatrn :: H.P.Pattern -> Maybe Bool
isBoolPatrn (H.P.Lit (H.V.Bool x)) = Just x
isBoolPatrn _ = Nothing




-- TODO: ...
builtin :: H.E.Expr -> Maybe S.S.Stmt

-- Tuples to Vectors
builtin (H.E.Tuple
    [ H.E.Lit (H.V.Float val1)
    , H.E.Lit (H.V.Float val2)
    ]) = Just $
            S.S.FunCall
                (S.ID.Ident' "vec2")
                [ S.S.Lit $ S.Lit.Float val1
                , S.S.Lit $ S.Lit.Float val2
                ]

builtin (H.E.Tuple
    [ H.E.Lit (H.V.Float val1)
    , H.E.Lit (H.V.Float val2)
    , H.E.Lit (H.V.Float val3)
    ]) = Just $
            S.S.FunCall
                (S.ID.Ident' "vec3")
                [ S.S.Lit $ S.Lit.Float val1
                , S.S.Lit $ S.Lit.Float val2
                , S.S.Lit $ S.Lit.Float val3
                ]

builtin (H.E.Tuple
    [ H.E.Lit (H.V.Float val1)
    , H.E.Lit (H.V.Float val2)
    , H.E.Lit (H.V.Float val3)
    , H.E.Lit (H.V.Float val4)
    ]) = Just $
            S.S.FunCall
                (S.ID.Ident' "vec4")
                [ S.S.Lit $ S.Lit.Float val1
                , S.S.Lit $ S.Lit.Float val2
                , S.S.Lit $ S.Lit.Float val3
                , S.S.Lit $ S.Lit.Float val4
                ]



builtin _ = Nothing


