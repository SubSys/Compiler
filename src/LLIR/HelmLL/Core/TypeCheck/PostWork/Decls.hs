{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns #-}
module LLIR.HelmLL.Core.TypeCheck.PostWork.Decls (
    finalize
) where


-- *
import Core
import Core.Control.Flow ((|>), (<|))
import Core.List.Util    (singleton)
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




-- + HelmLL AST Utils
import qualified LLIR.HelmLL.AST.Utils.Generic.Scope       as Scope
import qualified LLIR.HelmLL.AST.Utils.Class.Ident         as ID
import qualified LLIR.HelmLL.AST.Utils.Auxiliary.Type      as Ty
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

-- + Local
-- *


finalize :: [Decl.Function] -> [Decl.Function]
finalize = map (updateNameBinders . synthSudoFFIBinderTs)


updateNameBinders :: Decl.Function -> Decl.Function
updateNameBinders (Decl.Function (Etc.Binder ident Nothing) args block sig@(Just (T.Forall _ ty))) =
    Decl.Function
        (Etc.Binder ident (Just ty))
        args
        block
        sig


updateNameBinders x = x


synthSudoFFIBinderTs :: Decl.Function -> Decl.Function
synthSudoFFIBinderTs (SudoFFI.isSudoFFI' -> Just (Decl.Function name args expr scheme@(Just (T.Forall _ ty))))
    | List.length inTs /= List.length args =
        error "`synthSudoFFIBinderTs`: invalid type signature, or declaration."
    
    | otherwise =
        Decl.Function name args' expr scheme
    
    where
        inTs = Ty.getInputTypes ty
        args' = List.zipWith f args inTs
        
        f :: Etc.Binder -> T.Type -> Etc.Binder
        f (Etc.Binder ident Nothing) ty =
            Etc.Binder ident (Just ty)
        
        f binder@(Etc.Binder ident Just{}) _ = binder


synthSudoFFIBinderTs x = x