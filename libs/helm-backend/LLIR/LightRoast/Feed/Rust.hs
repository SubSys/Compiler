{-# LANGUAGE NoImplicitPrelude #-}
module LLIR.LightRoast.Feed.Rust (
    toRust
) where


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
import qualified Control.Monad.Writer       as M

import qualified Data.List     as List
import qualified Data.Text     as Text
import qualified Data.Map      as Map
import qualified Data.Set      as Set
import qualified Data.Foldable as Fold
import qualified Data.Monoid   as Monoid

import qualified Data.Generics.Uniplate.Data as Uni

--- Local Deps
import qualified LLIR.LightRoast.Data.Payload as LightRoast
import qualified CGIR.Rust.Data.Payload       as Rust

-- ~ (LLIR) LightRoast AST
-- ~~ Base
import qualified LLIR.LightRoast.AST.Base.Ident  as L.ID
import qualified LLIR.LightRoast.AST.Base.Types  as L.T
import qualified LLIR.LightRoast.AST.Base.Values as L.V
import qualified LLIR.LightRoast.AST.Base.Etc    as L.Etc
-- ~~ TermLevel
import qualified LLIR.LightRoast.AST.TermLevel.Stmt        as L.S
import qualified LLIR.LightRoast.AST.TermLevel.Patterns    as L.P
import qualified LLIR.LightRoast.AST.TermLevel.Block       as L.Decl
-- ~~ TopLevel
import qualified LLIR.LightRoast.AST.TopLevel.Functions as L.Decl
import qualified LLIR.LightRoast.AST.TopLevel.Unions    as L.Decl

-- ~ (CGIR) Rust AST
-- ~~ Base
import qualified CGIR.Rust.AST.Base.Ident  as R.ID
import qualified CGIR.Rust.AST.Base.Types  as R.T
import qualified CGIR.Rust.AST.Base.Values as R.V
import qualified CGIR.Rust.AST.Base.Etc    as R.Etc
-- ~~ TermLevel
import qualified CGIR.Rust.AST.TermLevel.Stmt        as R.S
import qualified CGIR.Rust.AST.TermLevel.Patterns    as R.P
import qualified CGIR.Rust.AST.TermLevel.Block       as R.Decl
-- ~~ TopLevel
import qualified CGIR.Rust.AST.TopLevel.Functions as R.Decl
import qualified CGIR.Rust.AST.TopLevel.Unions    as R.Decl


--- Local
import qualified LLIR.LightRoast.Feed.Rust.TopLevel.Functions as Decl
import qualified LLIR.LightRoast.Feed.Rust.TopLevel.Unions    as Decl
-- *



toRust :: LightRoast.Module -> Rust.Module
toRust payload =
    let functions = LightRoast.getFunctions payload
        unions    = LightRoast.getUnions payload
    in
        Rust.Module
            (map Decl.dropFunction functions)
            (map Decl.dropUnion unions)







