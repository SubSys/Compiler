{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
-- {-# LANGUAGE ViewPatterns #-}
module SLIR.HelmSyntax.Program.Core.Desugar.P3.Overloaded.Index (
    indexOverloads
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

-- + Local
import qualified SLIR.HelmSyntax.Program.Core.Desugar.P3.Data as P3
-- *








indexOverloads :: [Decl.Function] -> Map.Map ID.Ident (Map.Map T.Type Decl.Function)
indexOverloads input =
    input |> OL.getAndMapOverloads
          |> indexDecls



indexDecls :: [(ID.Ident, [Decl.Function])] -> Map.Map ID.Ident (Map.Map T.Type Decl.Function)
indexDecls = Map.fromList . map group
    where
        group :: (ID.Ident, [Decl.Function]) -> (ID.Ident, Map.Map T.Type Decl.Function)
        group (oldIdent, decls) =
            (oldIdent, Map.fromList $ List.zipWith (indexDecl oldIdent) [1..] decls)



indexDecl :: ID.Ident -> Int -> Decl.Function -> (T.Type, Decl.Function)
indexDecl oldIdent idx (Decl.Function name args expr sig meta)
    | Decl.Validated (T.Forall _ ty) _ <- sig =
        (ty, Decl.Function (newBinder idx) args expr sig meta)
    
    
    where
        newLabel x =
            Text.append
                (Text.append
                    (Text.pack "!!")
                    (Text.pack $ Pre.show x))
                (Text.append
                    (Text.pack "@")
                    (getText oldIdent)
                )
            
        newBinder =
            Etc.Binder_ . ID.Ident_ . newLabel


getText :: ID.Ident -> Text
getText (ID.Ident_ x) = x




