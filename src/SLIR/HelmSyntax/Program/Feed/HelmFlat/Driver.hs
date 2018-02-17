{-# LANGUAGE NoImplicitPrelude #-}
module SLIR.HelmSyntax.Program.Feed.HelmFlat.Driver (
    toHelmFlat
  , toHelmFlat'
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

-- + HelmSyntax & HelmFlat Interfaces
import qualified SLIR.HelmSyntax.Program.Data.Interface as HelmSyntax
import qualified HLIR.HelmFlat.Data.Interface           as HelmFlat

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
import qualified SLIR.HelmSyntax.Program.Feed.HelmFlat.Syntax as Syntax
-- *


toHelmFlat :: IO (Either Text HelmSyntax.Program) -> IO (Either Text HelmFlat.Program)
toHelmFlat upstream = do
    result <- upstream
    
    case result of
        Left err -> return $ Left err
        Right payload ->
            return
                $ Right
                $ toHelmFlat' payload


toHelmFlat' payload =
    let
        fns = HelmSyntax.getFunctions payload
            |> map Syntax.dropFunction
        uns = HelmSyntax.getUnions payload
            |> map Syntax.dropUnion
    in
        HelmFlat.Program
            { HelmFlat.unions = uns
            , HelmFlat.functions = fns
            }























