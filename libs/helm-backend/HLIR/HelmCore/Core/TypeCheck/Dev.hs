{-# LANGUAGE NoImplicitPrelude #-}
module HLIR.HelmCore.Core.TypeCheck.Dev where


-- *
import Core
import Core.Control.Flow ((|>), (<|))
import Core.List.Util    (flatten)

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


--- Dev
import qualified Dev.Samples.Basic      as BasicSamples
import qualified Dev.Samples.Complex    as ComplexSamples
import qualified Dev.Samples.TestParser as ParserSample

import qualified Text.Show.Prettyprint as PP
import qualified HLIR.HelmCore.Render.Utils as Display


-- ~ Upstream
import qualified SLIR.HelmSyntax.Core as HelmSyntax

--- Local Deps
-- ~ HelmCore IR
import qualified HLIR.HelmCore.Data.Payload as Payload

-- ~ HelmCore AST
-- ~~ Base
import qualified HLIR.HelmCore.AST.Base.Ident  as ID
import qualified HLIR.HelmCore.AST.Base.Types  as T
import qualified HLIR.HelmCore.AST.Base.Values as V
-- ~~ TermLevel
import qualified HLIR.HelmCore.AST.TermLevel.Expressions as E
import qualified HLIR.HelmCore.AST.TermLevel.Patterns    as P
-- ~~ TopLevel
import qualified HLIR.HelmCore.AST.TopLevel.Functions as Decl
import qualified HLIR.HelmCore.AST.TopLevel.Unions    as Decl

--- Local
import qualified HLIR.HelmCore.Core.TypeCheck.Driver as Driver
-- *

{-# ANN module ("HLint: ignore" :: String) #-}




upstream =
    ParserSample.sampleOne
        |> HelmSyntax.frontend
        |> HelmSyntax.toHelmCore
        |> Driver.typeCheck
        -- |> Driver.globalize


run = do
    input <- upstream
    
    case input of
        Left err      -> putStrLn (Text.unpack err)
        Right payload -> run' payload



run' payload =
    -- M.mapM_ PP.prettyPrint fns
    -- putStrLn $ Text.unpack (Display.renderFunctions functions)
    
    putStrLn $ Text.unpack (Display.renderFunctions functions)
    
    -- M.mapM_ PP.prettyPrint functions
    
    
    where
        
        functions  = Payload.getFunctions payload







-- *
-- | Resolve Infered Syntax
-- *


