{-# LANGUAGE NoImplicitPrelude #-}
module HLIR.HelmFlat.Feed.HelmLL.Driver (
    toHelmLL
  , toHelmLL'
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




-- + HelmFlat AST Interface
import qualified HLIR.HelmFlat.Data.Interface as HelmFlat
import qualified HLIR.HelmFlat.Data.Interface as I
-- + GLSL AST Interface
import qualified LLIR.HelmLL.Data.Interface   as HelmLL

-- + HelmFlat AST
-- ++ Base
import qualified HLIR.HelmFlat.AST.Data.Semantic.Base.Etc      as HL.Etc
import qualified HLIR.HelmFlat.AST.Data.Semantic.Base.Ident    as HL.ID
import qualified HLIR.HelmFlat.AST.Data.Semantic.Base.Types    as HL.T
import qualified HLIR.HelmFlat.AST.Data.Semantic.Base.Values   as HL.V

-- ++ TermLevel
import qualified HLIR.HelmFlat.AST.Data.Semantic.TermLevel.Expr     as HL.E
import qualified HLIR.HelmFlat.AST.Data.Semantic.TermLevel.Patterns as HL.P

-- ++ TopLevel
import qualified HLIR.HelmFlat.AST.Data.Semantic.TopLevel.Functions as HL.Decl
import qualified HLIR.HelmFlat.AST.Data.Semantic.TopLevel.Unions    as HL.Decl


-- + HelmLL AST
-- ++ Base
import qualified LLIR.HelmLL.AST.Data.Base.Etc      as LL.Etc
import qualified LLIR.HelmLL.AST.Data.Base.Ident    as LL.ID
import qualified LLIR.HelmLL.AST.Data.Base.Types    as LL.T
import qualified LLIR.HelmLL.AST.Data.Base.Literals as LL.Lit

-- ++ TermLevel
import qualified LLIR.HelmLL.AST.Data.TermLevel.Stmt     as LL.S
import qualified LLIR.HelmLL.AST.Data.TermLevel.Patterns as LL.P

-- ++ TopLevel
import qualified LLIR.HelmLL.AST.Data.TopLevel.Functions as LL.Decl
import qualified LLIR.HelmLL.AST.Data.TopLevel.Unions    as LL.Decl

-- + Local
import qualified HLIR.HelmFlat.Feed.HelmLL.Syntax.TopLevel.Functions as Decl
import qualified HLIR.HelmFlat.Feed.HelmLL.Syntax.TopLevel.Unions    as Decl
-- *





toHelmLL :: IO (Either Text I.Program) -> IO (Either Text HelmLL.Program)
toHelmLL upstream = do
    result <- upstream
    
    case result of
        Left err -> return $ Left err
        Right payload ->
            return
                $ Right
                $ toHelmLL' payload



toHelmLL' :: I.Program -> HelmLL.Program
toHelmLL' payload =
    let fns = I.getFunctions payload
            |> map Decl.dropFunction
        uns = I.getUnions payload
            |> map Decl.dropUnion
    in
        HelmLL.Program
            { HelmLL.unions = uns
            , HelmLL.functions = fns
            }



