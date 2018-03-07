{-# LANGUAGE NoImplicitPrelude #-}
-- {-# LANGUAGE ViewPatterns #-}
module LLIR.HelmLL.Core.Index.Syntax.Base.Type (
    indexScheme
  , indexType
  , typeBindable
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
    , (!!)
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



-- + HelmLL Module Interface
import qualified LLIR.HelmLL.Data.Interface as I

-- + HelmLL AST Utils
import qualified LLIR.HelmLL.AST.Utils.Generic.Scope       as Scope
import qualified LLIR.HelmLL.AST.Utils.Class.Ident         as ID
import qualified LLIR.HelmLL.AST.Utils.Auxiliary.Functions as Fn
import qualified LLIR.HelmLL.AST.Utils.Generic.SudoFFI     as SudoFFI

-- + HelmLL AST
-- ++ Base
import qualified LLIR.HelmLL.AST.Data.Base.Etc      as Etc
import qualified LLIR.HelmLL.AST.Data.Base.Ident    as ID
import qualified LLIR.HelmLL.AST.Data.Base.Types    as T
import qualified LLIR.HelmLL.AST.Data.Base.Literals   as V

-- ++ TermLevel
import qualified LLIR.HelmLL.AST.Data.TermLevel.Stmt     as S
import qualified LLIR.HelmLL.AST.Data.TermLevel.Patterns as P

-- ++ TopLevel
import qualified LLIR.HelmLL.AST.Data.TopLevel.Functions as Decl
import qualified LLIR.HelmLL.AST.Data.TopLevel.Unions    as Decl


-- + Local Prelude
import LLIR.HelmLL.Core.Index.Data.System (enter)

-- + Local
import qualified LLIR.HelmLL.Core.Index.Data.System     as Sys
import qualified LLIR.HelmLL.Core.Index.Scope.Referable as Scope
import qualified LLIR.HelmLL.Core.Index.Scope.Utils     as Scope
-- *



indexScheme :: T.Scheme -> Sys.Index T.Scheme
indexScheme (T.Forall as ty) = do
    (as', subs) <- List.unzip <$> M.mapM typeBindable as
    ty' <- Scope.withLocalSubst (Map.unions subs) (indexType ty)
    
    enter
        (T.Forall as' ty')
        (Map.unions subs)


indexType :: T.Type -> Sys.State T.Type
indexType = Uni.transformM f
    where
        f :: T.Type -> Sys.State T.Type
        f (T.Var ident) = do
            (ident', _) <- Scope.referable ident
            
            return (T.Var ident')

        f x = return x


typeBindable :: ID.Ident -> Sys.State (ID.Ident, Sys.Subst)
typeBindable binder = do
    idx <- Sys.incTypeCounter
    -- *

    -- *
    let (binder', subs) = newTypeSubst binder idx
    -- *

    -- *
    return (binder', subs)


newTypeSubst :: ID.Ident -> Int -> (ID.Ident, Sys.Subst)
newTypeSubst ident idx =
    let
        -- Finish
        newBinder = freshTypeIdent idx
        subs'     = Map.singleton ident newBinder

    in
        (newBinder, subs')


freshTypeIdent :: Int -> ID.Ident
freshTypeIdent i =
    ID.Ident' (upperLetters !! i)



upperLetters =
    [1..] >>= flip M.replicateM ['A'..'Z']


