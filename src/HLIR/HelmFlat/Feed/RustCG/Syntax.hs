{-# LANGUAGE NoImplicitPrelude #-}
module HLIR.HelmFlat.Feed.RustCG.Syntax (
    dropUnion
  , dropFunction
  , dropType
  , dropIdent
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

-- + RustCG AST
-- ++ Base
import qualified CGIR.Rust.AST.Data.Base.Ident                 as R.ID
import qualified CGIR.Rust.AST.Data.Base.Literals              as R.Lit
import qualified CGIR.Rust.AST.Data.Base.Types                 as R.T
import qualified CGIR.Rust.AST.Data.Base.Etc                   as R.Etc
-- ++ Block Level
import qualified CGIR.Rust.AST.Data.TermLevel.Stmt            as R.S
import qualified CGIR.Rust.AST.Data.TermLevel.Patterns        as R.P
-- ++ Decl/Top Level
import qualified CGIR.Rust.AST.Data.TopLevel.Enums.Variants   as R.Decl
import qualified CGIR.Rust.AST.Data.TopLevel.Enums            as R.Decl
import qualified CGIR.Rust.AST.Data.TopLevel.Functions        as R.Decl
-- *





-- {-# ANN module ("HLint: ignore" :: String) #-}



dropIdent :: H.ID.Ident -> R.ID.Ident
dropIdent (H.ID.Ident_ txt) = R.ID.Ident txt


dropLiteral :: H.V.LiteralValue -> R.Lit.LiteralValue
dropLiteral (H.V.Char val) = R.Lit.Char val
dropLiteral (H.V.String val) = R.Lit.String val
dropLiteral (H.V.Int val) = R.Lit.Int val
dropLiteral (H.V.Float val) = R.Lit.Float val
dropLiteral (H.V.Bool val) = R.Lit.Bool val

dropType :: H.T.Type -> R.T.Type
dropType H.T.String = R.T.String
dropType H.T.Char   = R.T.Char
dropType H.T.Int    = R.T.Int
dropType H.T.Float  = R.T.Float
dropType H.T.Bool   = R.T.Bool

dropType (H.T.List ty) =
    R.T.List (dropType ty)

dropType (H.T.Tuple ts) =
    R.T.Tuple
        (map dropType ts)

dropType (H.T.Union name args) =
    R.T.Union
        (toPath name)
        (map dropType args)

dropType (H.T.Var id') =
    R.T.Generic (dropIdent id')


dropType ty@H.T.Arr{} =
    let
        inputTypes = Type.getInputTypes ty
        outputType = Type.getReturnType ty
    in
        R.T.Fn_
            (map dropType inputTypes)
            (dropType outputType)




dropPattern :: H.P.Pattern -> R.P.Pattern

dropPattern (H.P.Lit lit) =
    R.P.Lit
        (dropLiteral lit)

dropPattern (H.P.List xs) =
    R.P.List
        (map dropPattern xs)

dropPattern (H.P.ListCons xs rest) =
    R.P.ListCons
        (map dropPattern xs)
        (Core.applyMaybe dropPattern rest)

dropPattern (H.P.Tuple items) =
    R.P.Tuple
        (map dropPattern items)

dropPattern (H.P.Constr ident args) =
    R.P.Variant
        (toPath ident)
        (map dropPattern args)

dropPattern (H.P.Var ident) =
    R.P.Var
        (binder2Ident ident)

dropPattern H.P.Wildcard =
    R.P.Wildcard



dropCaseAlt :: H.P.CaseAlt -> R.P.Arm
dropCaseAlt (H.P.CaseAlt patrn expr) =
    R.P.Arm
        (dropPattern patrn)
        (dropExpr expr)


dropExpr :: H.E.Expr -> R.S.Stmt
dropExpr (H.E.Lit val) =
    R.S.Lit
        (dropLiteral val)

dropExpr (H.E.Tuple items) =
    R.S.Tuple
        (map dropExpr items)

dropExpr (H.E.List xs) =
    R.S.List
        (map dropExpr xs)

dropExpr (H.E.ConCall ident []) =
    R.S.ConCall
        (toPath ident)
        []

dropExpr (H.E.Case con alts) =
    R.S.Match
        (dropExpr con)
        (map dropCaseAlt alts)

dropExpr (H.E.FunCall name args) =
    R.S.FunCall
        (toPath name)
        (map dropExpr args)

dropExpr (H.E.ConCall name args) =
    R.S.ConCall
        (toPath name)
        (map dropExpr args)

dropExpr (H.E.Ref name) =
    R.S.Ref
        (toPath name)


dropUnion :: H.Decl.Union -> R.Decl.Enum
dropUnion (H.Decl.Union name as cs) =
    R.Decl.Enum
        (dropIdent name)
        (map toGeneric as)
        (map dropConstructor cs)

dropConstructor :: H.Decl.Constructor -> R.Decl.Variant
dropConstructor (H.Decl.Constructor name args) =
    R.Decl.TupleVariant
        (dropIdent name)
        (map dropType args)




dropFunction :: H.Decl.Function -> R.Decl.Function
dropFunction (H.Decl.Function name args body (Just scheme@(H.T.Forall gs ty))) =
    R.Decl.Function
        (binder2Ident name)
        (map toGeneric gs)
        (map binder2Input args)
        (R.Etc.Output (dropType output))
        (R.S.Block [dropExpr body])
    
    where
        output = Type.getReturnType ty



-- | Helpers
--

toPath :: H.ID.Ident -> R.ID.Path
toPath (H.ID.Ident txt Nothing) = R.ID.Path_ [txt]
toPath (H.ID.Ident txt (Just (H.ID.Namespace segs))) =
    R.ID.Path_ (segs ++ [txt])

binder2Ident :: H.Etc.Binder -> R.ID.Ident
binder2Ident (H.Etc.Binder_ ident) = dropIdent ident


-- ref2Path :: H.Etc.Ref -> R.ID.Path
-- ref2Path (H.Etc.Ref ident) = toPath ident



toGeneric :: H.ID.Ident -> R.Etc.Generic
toGeneric (H.ID.Ident_ txt) =
    R.Etc.Generic (R.ID.Ident txt)



binder2Input :: H.Etc.Binder -> R.Etc.Input
binder2Input (H.Etc.Binder ident (Just ty)) =
    R.Etc.Input
        (dropIdent ident)
        (dropType ty)

