{-# LANGUAGE NoImplicitPrelude #-}
module CGIR.Rust.Core.Index.Data.System (
    OldName
  , NewName
  , Subst
  , State
  , Index
  , enter
  , binder
  , initCounter
  , incCounter
  , decCounter
  , runState
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

-- + Dev & Debugging
import qualified Text.Show.Prettyprint as PP



-- + RustCG AST Interface
import qualified CGIR.Rust.Data.Interface as I

-- + RustCG AST
-- ++ Base
import qualified CGIR.Rust.AST.Data.Base.Ident                 as ID
import qualified CGIR.Rust.AST.Data.Base.Literals              as Lit
import qualified CGIR.Rust.AST.Data.Base.Types                 as T
import qualified CGIR.Rust.AST.Data.Base.Etc                   as Etc
-- ++ Block Level
import qualified CGIR.Rust.AST.Data.TermLevel.Stmt            as S
import qualified CGIR.Rust.AST.Data.TermLevel.Patterns        as P
-- ++ Decl/Top Level
import qualified CGIR.Rust.AST.Data.TopLevel.Enums.Variants   as Decl
import qualified CGIR.Rust.AST.Data.TopLevel.Enums            as Decl
import qualified CGIR.Rust.AST.Data.TopLevel.Functions        as Decl
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
newtype Counter = Counter Int

type State a = M.StateT Counter (M.Reader Subst) a


type Index a = State (a, Subst)


{-
    ## Helpers
-}

enter :: a -> Index a
enter x = return (x, Map.empty)

binder :: a -> Subst -> Index a
binder x subs = return (x, subs)


initCounter = Counter 0


-- | Get & Increment/Decrement Counter
--
incCounter :: State Int
incCounter = do
    (Counter c) <- M.get
    
    
    M.put (Counter $ c + 1)
    
    return c


decCounter :: State Int
decCounter = do
    (Counter c) <- M.get
    
    M.put (Counter $ c - 1)
    
    return c




-- | Run State at specified counter (`Int`).
--
runState :: State a -> Int -> Subst -> a
runState m c s =
    fst $ M.runReader (M.runStateT m (Counter c)) s





