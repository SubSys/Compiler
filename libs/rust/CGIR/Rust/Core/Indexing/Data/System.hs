{-# LANGUAGE NoImplicitPrelude #-}
module CGIR.Rust.Core.Indexing.Data.System where


-- *
import Core
import Core.Control.Flow ((|>), (<|))
import Prelude (return, String, IO, show, error, (<$>))

import Data.List.Index  (imap)

import qualified Control.Monad.State        as M
import qualified Control.Monad.Except       as M
import qualified Control.Monad.RWS          as M
import qualified Control.Monad.Identity     as M
import qualified Control.Monad.Reader       as M

import qualified Data.List     as List
import qualified Data.Text     as Text
import qualified Data.Map      as Map
import qualified Data.Set      as Set
import qualified Data.Foldable as Fold
import qualified Data.Monoid   as Monoid

import qualified Data.Generics.Uniplate.Data as Uni


--- Local Deps
-- ~ (GCIR) - Rust AST
-- ~~ Base
import qualified CGIR.Rust.AST.Base.Ident  as ID
import qualified CGIR.Rust.AST.Base.Types  as T
import qualified CGIR.Rust.AST.Base.Values as V
import qualified CGIR.Rust.AST.Base.Etc    as Etc
-- ~~ TermLevel
import qualified CGIR.Rust.AST.TermLevel.Stmt        as S
import qualified CGIR.Rust.AST.TermLevel.Patterns    as P
import qualified CGIR.Rust.AST.TermLevel.Block       as Decl
-- ~~ TopLevel
import qualified CGIR.Rust.AST.TopLevel.Functions as Decl
import qualified CGIR.Rust.AST.TopLevel.Unions    as Decl

--- Local
import qualified CGIR.Rust.Core.Indexing.Data.Subst as Sub
-- *




newtype Counter = Counter {count :: Int}

data IndexingMode
    = Localizing
    | Globalizing
    deriving (Show)


type State a = M.StateT Counter (M.Reader (IndexingMode, Sub.Subst)) a


-- *
-- | # Indexable
-- *
type Index a = State (a, Sub.Subst)

class Indexable a where
    index :: a -> Index a



-- *
-- | ## Indexable - Helpers
-- *

enter :: a -> Sub.Subst -> Index a
enter a s =
    return (a, s)


-- *
-- | Misc. Utils
-- *

initCounter = 
    Counter {count = 0}


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


getIndexingMode :: State IndexingMode
getIndexingMode =
    fst <$> M.ask



-- | Run State at specified counter (`Int`).
--
runState :: IndexingMode -> State a -> Int -> Sub.Subst -> a
runState mode m c s =
    fst $ M.runReader (M.runStateT m (Counter c)) (mode, s)




