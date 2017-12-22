{-# LANGUAGE NoImplicitPrelude #-}
module HLIR.HelmIntro.Core.Program.Specialize.Data.System where


-- *
import Core
import Core.Control.Flow ((|>), (<|))
import Core.List.Util    (flatten)

import Prelude (return, String, IO, show, error, (<$>), (>>))

import Data.List.Index  (imap)

import qualified Control.Monad.State        as M
import qualified Control.Monad.Except       as M
import qualified Control.Monad.RWS          as M
import qualified Control.Monad.Identity     as M
import qualified Control.Monad.Reader       as M
import qualified Control.Monad.Trans.Maybe  as M
import qualified Control.Monad.Writer       as M

import qualified Data.List     as List
import qualified Data.Text     as Text
import qualified Data.Map      as Map
import qualified Data.Set      as Set
import qualified Data.Foldable as Fold
import qualified Data.Monoid   as Monoid

import qualified Data.Generics.Uniplate.Data as Uni
import qualified Text.Show.Prettyprint as PP


-- ~ HelmSyntax Misc.
import qualified HLIR.HelmIntro.Render.Utils as Display

-- ~ HelmSyntax IR
import qualified HLIR.HelmIntro.Data.Payload as Payload

--- Local Deps
-- ~ HelmSyntax AST
-- ~~ Base
import qualified HLIR.HelmIntro.AST.Data.Base.Etc      as Etc
import qualified HLIR.HelmIntro.AST.Data.Base.Ident    as ID
import qualified HLIR.HelmIntro.AST.Data.Base.Types    as T
import qualified HLIR.HelmIntro.AST.Data.Base.Values   as V
import qualified HLIR.HelmIntro.AST.Data.Base.Metadata as Meta

-- ~~ TermLevel
import qualified HLIR.HelmIntro.AST.Data.TermLevel.Expressions as E
import qualified HLIR.HelmIntro.AST.Data.TermLevel.Patterns    as P

-- ~~ TopLevel
import qualified HLIR.HelmIntro.AST.Data.TopLevel.Fixities  as Decl
import qualified HLIR.HelmIntro.AST.Data.TopLevel.Functions as Decl
import qualified HLIR.HelmIntro.AST.Data.TopLevel.Unions    as Decl

-- ~~ Header
import qualified HLIR.HelmIntro.AST.Data.Header.Base       as Base
import qualified HLIR.HelmIntro.AST.Data.Header.ImportDecl as Decl

-- ~~ Auxiliary Nodes
import qualified HLIR.HelmIntro.AST.Auxiliary.Canonical.Ident as CID


--- Local
import qualified HLIR.HelmIntro.Core.Program.Specialize.Data.Env as Env
-- *




-- type Env a = M.Reader 

type Special a = M.WriterT [Decl.Function] (M.Reader Env.Env) (Maybe a)




