{-# LANGUAGE NoImplicitPrelude #-}
module CGIR.GLSL.AST.Utils.Generic.SudoFFI (
    isSudoFFI
) where


-- *
import Core
import Core.Control.Flow ((|>), (<|))
import Core.List.Util    (flatten, singleton)
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
import qualified Data.Data                    as Data
import qualified Data.String                  as String

-- + Recursion Schemes & Related
import qualified Data.Functor.Foldable       as F
import qualified Data.Generics.Uniplate.Data as Uni

-- + OS APIS & Related
import qualified System.IO as SIO

-- + Dev & Debugging
import qualified Text.Show.Prettyprint as PP



-- + GLSL AST Interface
import qualified CGIR.GLSL.Data.Interface as I

-- + GLSL AST
-- ++ Base
import qualified CGIR.GLSL.AST.Data.Base.Ident                 as ID
import qualified CGIR.GLSL.AST.Data.Base.Literals              as Lit
import qualified CGIR.GLSL.AST.Data.Base.Types                 as T
import qualified CGIR.GLSL.AST.Data.Base.Etc                   as Etc
-- ++ Block Level
import qualified CGIR.GLSL.AST.Data.TermLevel.Stmt            as S
-- ++ Decl/Top Level
import qualified CGIR.GLSL.AST.Data.TopLevel.Functions         as Decl
import qualified CGIR.GLSL.AST.Data.TopLevel.Globals           as Decl

-- + Local
import qualified CGIR.GLSL.AST.Render.Syntax.Base.Ident         as ID
import qualified CGIR.GLSL.AST.Render.Syntax.Base.Etc           as Etc
import qualified CGIR.GLSL.AST.Render.Syntax.Base.Literals      as Lit
import qualified CGIR.GLSL.AST.Render.Syntax.Base.Types         as T
import qualified CGIR.GLSL.AST.Render.Syntax.BlockLevel.Stmt    as S
import qualified CGIR.GLSL.AST.Render.Syntax.TopLevel.Functions as Decl
import qualified CGIR.GLSL.AST.Render.Syntax.TopLevel.Globals   as Decl
-- *






specialNamespace :: ID.Namespace
specialNamespace =
    ID.Namespace [Text.pack "Helm", Text.pack "Compiler", Text.pack "Sudo", Text.pack "Native"]


isSudoFFI :: (Data.Data a, Data.Typeable a) => a -> Bool
isSudoFFI input = List.any pred [ ns | (ID.Ident _ (Just ns)) <- Uni.universeBi input ]
    where
        pred :: ID.Namespace -> Bool
        pred x =
            x == specialNamespace
