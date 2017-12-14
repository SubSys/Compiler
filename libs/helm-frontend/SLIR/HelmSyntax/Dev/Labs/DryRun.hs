{-# LANGUAGE NoImplicitPrelude #-}
module SLIR.HelmSyntax.Dev.Labs.DryRun where


-- *
import Core
import Core.Control.Flow ((|>), (<|))
import Prelude (return, String, IO, show, error)

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
import qualified SLIR.HelmSyntax.Render.Utils as Display

-- ~ HelmSyntax Cores
import qualified SLIR.HelmSyntax.Core.Parser.Driver    as Driver
import qualified SLIR.HelmSyntax.Core.TypeCheck.Driver as Driver
import qualified SLIR.HelmSyntax.Core.Desugar.Driver   as Driver

-- ~ HelmSyntax IR
import qualified SLIR.HelmSyntax.Data.Payload as Payload

--- Local Deps
-- ~ HelmSyntax AST
-- ~~ Base
import qualified SLIR.HelmSyntax.AST.Data.Base.Etc    as Etc
import qualified SLIR.HelmSyntax.AST.Data.Base.Ident  as ID
import qualified SLIR.HelmSyntax.AST.Data.Base.Types  as T
import qualified SLIR.HelmSyntax.AST.Data.Base.Values as V

-- ~~ TermLevel
import qualified SLIR.HelmSyntax.AST.Data.TermLevel.Expressions as E
import qualified SLIR.HelmSyntax.AST.Data.TermLevel.Patterns    as P

-- ~~ TopLevel
import qualified SLIR.HelmSyntax.AST.Data.TopLevel.Fixities  as Decl
import qualified SLIR.HelmSyntax.AST.Data.TopLevel.Functions as Decl
import qualified SLIR.HelmSyntax.AST.Data.TopLevel.Unions    as Decl
-- *



{-# ANN module "HLint: ignore" #-}


upstream =
    ParserSample.sampleOne
        |> Driver.parser
        |> Driver.typeCheck
        -- |> Driver.desugar

run = do
    result <- upstream
    
    case result of
        Left err      -> putStrLn (Text.unpack err)
        Right payload -> run' payload



run' payload =
    -- M.mapM_ PP.prettyPrint fns
    -- putStrLn $ Text.unpack (Display.renderFunctions functions)
    
    putStrLn $ Text.unpack (Display.renderFunctions functions)
    
    -- M.mapM_ PP.prettyPrint functions
    
    
    where
        functions  = Payload.getFunctions payload


