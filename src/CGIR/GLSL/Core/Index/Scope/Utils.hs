{-# LANGUAGE NoImplicitPrelude #-}
module CGIR.GLSL.Core.Index.Scope.Utils (
    withLocalSubst
  , indexList
  , indexMaybe
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
import qualified CGIR.GLSL.Core.Index.Data.System as Sys
-- *


withLocalSubst :: Sys.Subst -> Sys.State a -> Sys.State a
withLocalSubst s1 =
    M.local modEnv
    
    where
        
        modEnv = Map.union s1


indexList :: (a -> Sys.Index a) -> [a] -> Sys.Index [a]
indexList f []     = Sys.enter [] Map.empty

indexList f (x:xs) = do
    (x', s)   <- f x
    (xs', ss) <- withLocalSubst s (indexList f xs)
    
    return (x' : xs', Map.union ss s)


indexMaybe :: (a -> Sys.Index a) -> Maybe a -> Sys.Index (Maybe a)
indexMaybe f Nothing = Sys.enter Nothing Map.empty
indexMaybe f (Just x) = do
    (x', s) <- f x

    Sys.enter (Just x') s





