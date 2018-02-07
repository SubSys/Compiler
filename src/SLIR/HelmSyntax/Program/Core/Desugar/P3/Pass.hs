{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns #-}
module SLIR.HelmSyntax.Program.Core.Desugar.P3.Pass (
    desugarP3
) where


-- *
import Core
import Core.Control.Flow ((|>), (<|))
import Core.List.Util    (flatten, singleton)
import Data.Monoid ((<>))
import Prelude
    (return
    , String
    , IO
    , show
    , error
    , (<$>)
    , (>>=)
    , (>>)
    , fromIntegral
    )

import qualified Data.Data as Data

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

-- + HelmSyntax Module Interface
import qualified SLIR.HelmSyntax.Program.Data.Interface as I

-- + HelmSyntax AST Renderer
import qualified SLIR.HelmSyntax.AST.Render.Syntax.Driver as Syntax

-- + HelmSyntax AST Utils
import qualified SLIR.HelmSyntax.AST.Utils.Scope                         as Scope
import qualified SLIR.HelmSyntax.AST.Utils.Auxiliary.Ident               as ID
import qualified SLIR.HelmSyntax.AST.Utils.Auxiliary.Functions.SudoFFI   as SudoFFI
import qualified SLIR.HelmSyntax.AST.Utils.Auxiliary.Functions.Recursive as Rec
import qualified SLIR.HelmSyntax.AST.Utils.Auxiliary.Type                as Type
import qualified SLIR.HelmSyntax.AST.Utils.Auxiliary.Binders             as Binder
import qualified SLIR.HelmSyntax.AST.Utils.Auxiliary.Expr                as Expr

import qualified SLIR.HelmSyntax.AST.Utils.Auxiliary.Special.Overloaded as OL
import qualified SLIR.HelmSyntax.AST.Utils.Auxiliary.Special.Superposed as SP

-- + HelmSyntax AST
-- ++ Base
import qualified SLIR.HelmSyntax.AST.Data.Semantic.Base.Etc      as Etc
import qualified SLIR.HelmSyntax.AST.Data.Semantic.Base.Ident    as ID
import qualified SLIR.HelmSyntax.AST.Data.Semantic.Base.Types    as T
import qualified SLIR.HelmSyntax.AST.Data.Semantic.Base.Values   as V
import qualified SLIR.HelmSyntax.AST.Data.Semantic.Base.Metadata as Meta
import qualified SLIR.HelmSyntax.AST.Data.Semantic.Base.Header   as Header

-- ++ TermLevel
import qualified SLIR.HelmSyntax.AST.Data.Semantic.TermLevel.Expr     as E
import qualified SLIR.HelmSyntax.AST.Data.Semantic.TermLevel.Patterns as P

-- ++ TopLevel
import qualified SLIR.HelmSyntax.AST.Data.Semantic.TopLevel.Fixities  as Decl
import qualified SLIR.HelmSyntax.AST.Data.Semantic.TopLevel.Functions as Decl
import qualified SLIR.HelmSyntax.AST.Data.Semantic.TopLevel.Unions    as Decl

-- HelmSyntax - Program Drivers
import qualified SLIR.HelmSyntax.Program.Core.Uncurry.Driver   as Driver
import qualified SLIR.HelmSyntax.Program.Core.TypeCheck.Driver as Driver

-- + Local
import qualified SLIR.HelmSyntax.Program.Core.Desugar.P3.Data             as P3
import qualified SLIR.HelmSyntax.Program.Core.Desugar.P3.Overloaded.Index as IndexOL
-- *





desugarP3 :: I.Program -> I.Program
desugarP3 payload =
    payload |> Driver.uncurryTerms'
            |> Driver.typeCheck'
            |> pipeline
            |> Driver.typeCheck'




pipeline payload@(I.getFunctions -> decls) =
    payload |> I.updateFunctions (pipeline' decls)


pipeline' decls =
    let
        overloads    = IndexOL.indexOverloads decls
        
        -- TODO: Also for overloads that call overloads...
        nonOverloads = OL.onlyNonOverloads decls
        
        resolvedNormals = Uni.transformBi (exprScheme overloads) nonOverloads
    in
        postProcess overloads ++ resolvedNormals
    
    
    where
        -- Puts new decls into program scope...
        postProcess :: P3.IndexedOverloads -> [Decl.Function]
        postProcess (Map.elems -> groups) = flatten $ map Map.elems groups



exprScheme :: P3.IndexedOverloads -> E.Expr -> E.Expr
exprScheme ols expr@(E.FunCall ident args ty meta)
    | Just callSites <- Map.lookup ident ols =
        let
            (Just targetType) = Meta.overloadedTargetType meta
        in
            case Map.lookup targetType callSites of
                Nothing -> expr
                Just fn ->
                    let
                        newName = ID.get fn
                    in
                        E.FunCall newName args ty meta


exprScheme _ e = e











