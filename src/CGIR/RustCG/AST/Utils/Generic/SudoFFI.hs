{-# LANGUAGE NoImplicitPrelude #-}
module CGIR.RustCG.AST.Utils.Generic.SudoFFI (
    isSudoFFI
) where


-- *
import Core
import Core.Control.Flow ((|>), (<|))
import Core.List.Util    (flatten, singleton)
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

-- + Recursion Schemes & Related
import qualified Data.Functor.Foldable       as F
import qualified Data.Generics.Uniplate.Data as Uni

-- + OS APIS & Related
import qualified System.IO as SIO

-- + Frameworks
import Framework.Text.Renderer
import qualified Framework.Text.Renderer.Utils as Util
import qualified Text.PrettyPrint.Leijen.Text  as P

-- + Dev & Debugging
import qualified Text.Show.Prettyprint as PP



-- + RustCG AST Interface
import qualified CGIR.RustCG.Data.Interface as I

-- + RustCG AST
-- ++ Base
import qualified CGIR.RustCG.AST.Data.Semantic.Base.Ident                 as ID
import qualified CGIR.RustCG.AST.Data.Semantic.Base.Literals              as Lit
import qualified CGIR.RustCG.AST.Data.Semantic.Base.Types                 as T
import qualified CGIR.RustCG.AST.Data.Semantic.Base.Etc                   as Etc
-- ++ Block Level
import qualified CGIR.RustCG.AST.Data.Semantic.BlockLevel.Stmt            as S
import qualified CGIR.RustCG.AST.Data.Semantic.BlockLevel.Patterns        as P
-- ++ Decl/Top Level
import qualified CGIR.RustCG.AST.Data.Semantic.DeclLevel.Enums.Variants   as Decl
import qualified CGIR.RustCG.AST.Data.Semantic.DeclLevel.Enums            as Decl
import qualified CGIR.RustCG.AST.Data.Semantic.DeclLevel.Functions        as Decl

-- + Local
import qualified CGIR.RustCG.AST.Utils.Ident as ID
-- *


specialNamespace :: [Text]
specialNamespace =
    [Text.pack "Helm", Text.pack "Compiler", Text.pack "Sudo", Text.pack "Native"]


isSudoFFI :: (Data.Data a, Data.Typeable a) => a -> Bool
isSudoFFI input = List.any isSudoPath [ x | x@ID.Path{} <- Uni.universeBi input ]


isSudoPath :: ID.Path -> Bool
isSudoPath (ID.Path segs)
    | List.length segs >= 2 =
        let prefixs = List.init segs
                |> map toText
        in
            prefixs == specialNamespace
    
    | otherwise =
        False


toText :: ID.Seg -> Text
toText (ID.Seg _ x) = x











