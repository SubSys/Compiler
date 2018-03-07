{-# LANGUAGE NoImplicitPrelude #-}
module LLIR.HelmLL.Core.Index.Data.System (
    OldName
  , NewName
  , Subst
  , State
  , Index
  , enter
  , initCounter
  , incTermCounter
  , decTermCounter
  
  , incTypeCounter
  , resetTypeCounter
  
  , runState
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



-- + HelmLL Module Interface
import qualified LLIR.HelmLL.Data.Interface as I

-- + HelmLL AST Utils
import qualified LLIR.HelmLL.AST.Utils.Generic.Scope       as Scope
import qualified LLIR.HelmLL.AST.Utils.Class.Ident         as ID
import qualified LLIR.HelmLL.AST.Utils.Auxiliary.Functions as Fun
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
-- *




{-
    # Substitutions
-}
type OldName = ID.Ident
type NewName = ID.Ident

type Subst = Map.Map OldName NewName



{-
    # State Stuff
-}
newtype TermCounter = TermCounter Int
newtype TypeCounter = TypeCounter Int

type State a = M.StateT (TermCounter, TypeCounter) (M.Reader Subst) a


type Index a = State (a, Subst)


{-
    ## Helpers
-}

enter :: a -> Subst -> Index a
enter x subs = return (x, subs)


initCounter = (TermCounter 0, TypeCounter 0)




-- | Get & Increment/Decrement Counter
--
incTermCounter :: State Int
incTermCounter = do
    (TermCounter c, x) <- M.get
    
    
    M.put (TermCounter $ c + 1, x)
    
    return c


decTermCounter :: State Int
decTermCounter = do
    (TermCounter c, x) <- M.get
    
    M.put (TermCounter $ c - 1, x)
    
    return c



incTypeCounter :: State Int
incTypeCounter = do
    (x, TypeCounter c) <- M.get
    
    
    M.put (x, TypeCounter $ c + 1)
    
    return c


resetTypeCounter :: State ()
resetTypeCounter = do
    (x, _) <- M.get
    
    
    M.put (x, TypeCounter 0)


-- | Run State at specified counter (`Int`).
--
runState :: State a -> Int -> Subst -> a
runState m c s =
    fst $ M.runReader (M.runStateT m initCounter) s

