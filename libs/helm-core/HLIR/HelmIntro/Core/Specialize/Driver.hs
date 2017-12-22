{-# LANGUAGE NoImplicitPrelude #-}
module HLIR.HelmIntro.Core.Program.Specialize.Driver (
    specialize
) where


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


--- Local
import qualified HLIR.HelmIntro.Core.Program.Specialize.Init.Overloads as Overload
-- *




specialize :: Payload.Program -> Payload.Program
specialize payload =
    let
        -- Setup Data
        fns = Payload.getFunctions payload
        uns = Payload.getUnions payload
        
        -- Initial Data
        overloads = Overload.initOverloads fns
        
        renderOverloads =
            map Display.renderFunctions overloads
                |> Text.unlines
                |> Text.unpack
        
        renderProgram =
            Display.renderFunctions fns
                |> Text.unpack
        
        bar =
                   List.replicate 100 '-'
                ++ "\n"
                ++ List.replicate 100 '-'
                ++ "\n"
                ++ List.replicate 100 '-'
                ++ "\n\n"

    in
        error
            $ "\n" ++ renderOverloads ++ "\n" ++ bar ++ "\n" ++ renderProgram




-- process :: Decl.Function -> Decl





