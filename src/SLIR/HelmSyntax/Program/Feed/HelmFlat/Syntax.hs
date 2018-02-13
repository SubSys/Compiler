{-# LANGUAGE NoImplicitPrelude #-}
module SLIR.HelmSyntax.Program.Feed.HelmFlat.Syntax (
    dropFunction
  , dropUnion
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

-- + HelmSyntax AST
-- ++ Base
import qualified SLIR.HelmSyntax.AST.Data.Semantic.Base.Etc      as HS.Etc
import qualified SLIR.HelmSyntax.AST.Data.Semantic.Base.Ident    as HS.ID
import qualified SLIR.HelmSyntax.AST.Data.Semantic.Base.Types    as HS.T
import qualified SLIR.HelmSyntax.AST.Data.Semantic.Base.Values   as HS.V
import qualified SLIR.HelmSyntax.AST.Data.Semantic.Base.Metadata as HS.Meta

-- ++ TermLevel
import qualified SLIR.HelmSyntax.AST.Data.Semantic.TermLevel.Expr     as HS.E
import qualified SLIR.HelmSyntax.AST.Data.Semantic.TermLevel.Patterns as HS.P

-- ++ TopLevel
import qualified SLIR.HelmSyntax.AST.Data.Semantic.TopLevel.Fixities  as HS.Decl
import qualified SLIR.HelmSyntax.AST.Data.Semantic.TopLevel.Functions as HS.Decl
import qualified SLIR.HelmSyntax.AST.Data.Semantic.TopLevel.Unions    as HS.Decl


-- + HelmFlat AST
-- ++ Base
import qualified HLIR.HelmFlat.AST.Data.Semantic.Base.Etc      as HF.Etc
import qualified HLIR.HelmFlat.AST.Data.Semantic.Base.Ident    as HF.ID
import qualified HLIR.HelmFlat.AST.Data.Semantic.Base.Types    as HF.T
import qualified HLIR.HelmFlat.AST.Data.Semantic.Base.Values   as HF.V

-- ++ TermLevel
import qualified HLIR.HelmFlat.AST.Data.Semantic.TermLevel.Expr     as HF.E
import qualified HLIR.HelmFlat.AST.Data.Semantic.TermLevel.Patterns as HF.P

-- ++ TopLevel
import qualified HLIR.HelmFlat.AST.Data.Semantic.TopLevel.Functions as HF.Decl
import qualified HLIR.HelmFlat.AST.Data.Semantic.TopLevel.Unions    as HF.Decl


-- + Local
-- *




namespace :: Maybe HS.ID.Namespace -> Maybe HF.ID.Namespace
namespace Nothing = Nothing
namespace (Just (HS.ID.Namespace segs)) =
    Just $ HF.ID.Namespace segs

dropIdent :: HS.ID.Ident -> HF.ID.Ident
dropIdent (HS.ID.Ident' txt ns) =
    HF.ID.Ident txt (namespace ns)

toRef :: HS.ID.Ident -> HF.Etc.Ref
toRef ident =
    HF.Etc.Ref (dropIdent ident)

dropType :: HS.T.Type -> HF.T.Type

dropType (HS.T.Tuple ts _) =
    HF.T.Tuple (map dropType ts)

dropType (HS.T.List ty _) =
    HF.T.List (dropType ty)

dropType (HS.T.Union name args _) =
    HF.T.Union
        (dropIdent name)
        (map dropType args)

dropType (HS.T.Var id' _) =
    HF.T.Var (dropIdent id')

dropType (HS.T.Arr ty1 ty2 _) =
    HF.T.Arr
        (dropType ty1)
        (dropType ty2)

dropType (HS.T.Parens ty _) =
    dropType ty

dropType (HS.T.String _) =
    HF.T.String

dropType (HS.T.Char _) =
    HF.T.Char

dropType (HS.T.Int _) =
    HF.T.Int

dropType (HS.T.Float _) =
    HF.T.Float

dropType (HS.T.Bool _) =
    HF.T.Bool


dropScheme :: HS.T.Scheme -> HF.T.Scheme
dropScheme (HS.T.Forall as t) =
    HF.T.Forall
        (map dropIdent as)
        (dropType t)

dropBinder :: HS.Etc.Binder -> HF.Etc.Binder
dropBinder (HS.Etc.Binder ident Nothing)   = HF.Etc.Binder_ (dropIdent ident)
dropBinder (HS.Etc.Binder ident (Just ty)) =
    HF.Etc.Binder (dropIdent ident) (Just $ dropType ty)


dropValue :: HS.V.LiteralValue -> HF.V.LiteralValue
dropValue (HS.V.Char val _)   = HF.V.Char val
dropValue (HS.V.String val _) = HF.V.String val
dropValue (HS.V.Int val _)    = HF.V.Int val
dropValue (HS.V.Float val _)  = HF.V.Float val
dropValue (HS.V.Bool val _)   = HF.V.Bool val


dropPattern (HS.P.Lit lit _) =
    HF.P.Lit (dropValue lit)

dropPattern (HS.P.List xs _) =
    HF.P.List
        (map dropPattern xs)

dropPattern (HS.P.ListCons xs rest _) =
    HF.P.ListCons
        (map dropPattern xs)
        (Core.applyMaybe dropPattern rest)

dropPattern (HS.P.Tuple items _) =
    HF.P.Tuple
        (map dropPattern items)

dropPattern (HS.P.Constr ident args _) =
    HF.P.Constr
        (dropIdent ident)
        (map dropPattern args)

dropPattern (HS.P.Var arg _) =
    HF.P.Var
        (dropBinder arg)

dropPattern (HS.P.Wildcard _) =
    HF.P.Wildcard

dropCaseAlt :: HS.P.CaseAlt -> HF.P.CaseAlt
dropCaseAlt (HS.P.CaseAlt patrn expr _) =
    HF.P.CaseAlt
        (dropPattern patrn)
        (dropExpr expr)



dropExpr :: HS.E.Expr -> HF.E.Expr


dropExpr (HS.E.Lit val _) =
    HF.E.Lit
        (dropValue val)

dropExpr (HS.E.Tuple items _) =
    HF.E.Tuple
        (map dropExpr items)

dropExpr (HS.E.List xs _) =
    HF.E.List
        (map dropExpr xs)

dropExpr (HS.E.Constr ident _) =
    HF.E.Constr
        (dropIdent ident)


dropExpr (HS.E.Case con alts _) =
    HF.E.Case
        (dropExpr con)
        (map dropCaseAlt alts)


dropExpr (HS.E.FunCall name args ty _) =
    HF.E.FunCall
        (toRef name)
        (map dropExpr args)

dropExpr (HS.E.ConCall name args ty _) =
    HF.E.ConCall
        (dropIdent name)
        (map dropExpr args)

-- ... ?
dropExpr (HS.E.Parens expr _) = dropExpr expr

dropExpr (HS.E.Var name _) =
    HF.E.FunCall
        (toRef name)
        []

-- | Should be removed after desugaring.
--
dropExpr (HS.E.App e1 e2 _)          = error "Should have been removed after desugaring."
dropExpr (HS.E.Abs arg expr _)       = error "Should have been removed after desugaring."
dropExpr (HS.E.InfixApp sym e1 e2 _) = error "Should have been removed after desugaring."
dropExpr (HS.E.If intros elseExpr _) = error "Should have been removed after desugaring."
dropExpr (HS.E.Let fns expr _)       = error "Should have been removed after desugaring."




dropFunction :: HS.Decl.Function -> HF.Decl.Function
dropFunction (HS.Decl.Function name args expr sig _) =
    case sig of
        (HS.Decl.Validated scheme _) ->
            HF.Decl.Function
                (dropBinder name)
                (map dropBinder args)
                (dropExpr expr)
                (Just $ dropScheme scheme)
        
        _ ->
            HF.Decl.Function
                (dropBinder name)
                (map dropBinder args)
                (dropExpr expr)
                Nothing



dropUnion :: HS.Decl.Union -> HF.Decl.Union
dropUnion (HS.Decl.Union name as cs _) =
    HF.Decl.Union
        (dropIdent name)
        (map dropIdent as)
        (map dropConstructor cs)

dropConstructor :: HS.Decl.Constructor -> HF.Decl.Constructor
dropConstructor (HS.Decl.Constructor name args _) =
    HF.Decl.Constructor
        (dropIdent name)
        (map dropType args)
    


