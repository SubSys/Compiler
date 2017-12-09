{-# LANGUAGE NoImplicitPrelude #-}
module CGIR.Rust.Core.Indexing.Driver.Globalize (
    globalize
) where


-- *
import Core
import Core.Control.Flow ((|>), (<|))
import Prelude (return, String, IO, show, error, (<$>))

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
import qualified Text.Show.Prettyprint as PP


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
import qualified CGIR.Rust.Core.Indexing.Data.Subst            as Sub
import qualified CGIR.Rust.Core.Indexing.Data.System           as Sys
import qualified CGIR.Rust.Core.Indexing.Data.System.Bindable  as Bind
import qualified CGIR.Rust.Core.Indexing.Data.System.Referable as Ref
import qualified CGIR.Rust.Core.Indexing.Syntax                as Syntax
-- *


globalize :: [Decl.Function] -> [Decl.Function]
globalize fns =
    fst exec
    where
        exec = Sys.runState
                    Sys.Globalizing
                    (Syntax.indexer fns)
                    0
                    Sub.empty




