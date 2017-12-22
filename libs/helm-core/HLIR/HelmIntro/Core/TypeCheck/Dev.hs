{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns #-}
module HLIR.HelmIntro.Core.TypeCheck.Dev where


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
import qualified HLIR.HelmIntro.Render.Utils as Display

-- ~ HelmSyntax Cores
import qualified HLIR.HelmIntro.Core.Module.Parser.Driver    as Driver
import qualified HLIR.HelmIntro.Core.TypeCheck.Driver as Driver
import qualified HLIR.HelmIntro.Core.Desugar.Driver   as Driver

-- ~ HelmSyntax IR
import qualified HLIR.HelmIntro.Data.Interface.Module.Payload as Payload


--- Local Deps
-- ~ HelmSyntax AST
-- ~~ Base
import qualified HLIR.HelmIntro.AST.Data.Base.Etc    as Etc
import qualified HLIR.HelmIntro.AST.Data.Base.Ident  as ID
import qualified HLIR.HelmIntro.AST.Data.Base.Types  as T
import qualified HLIR.HelmIntro.AST.Data.Base.Values as V
import qualified HLIR.HelmIntro.AST.Data.Base.Metadata as Meta

-- ~~ TermLevel
import qualified HLIR.HelmIntro.AST.Data.TermLevel.Expressions as E
import qualified HLIR.HelmIntro.AST.Data.TermLevel.Patterns    as P

-- ~~ TopLevel
import qualified HLIR.HelmIntro.AST.Data.TopLevel.Fixities  as Decl
import qualified HLIR.HelmIntro.AST.Data.TopLevel.Functions as Decl
import qualified HLIR.HelmIntro.AST.Data.TopLevel.Unions    as Decl

-- ~ HelmSyntax AST Utils
import qualified HLIR.HelmIntro.AST.Toolbox.TopLevel.Functions.Overloaded as Overloaded

--- Local
import qualified HLIR.HelmIntro.Core.TypeCheck.Data.Interface.TypesEnv     as TI
-- *

{-# ANN module "HLint: ignore" #-}



upstream =
    let filePath   = Text.pack ParserSample.sampleOnePath
        sourceCode = ParserSample.sampleOne
    in
        sourceCode
            |> Driver.runModuleParser filePath
            |> Driver.typeCheck



run = do
    input <- upstream
    
    case input of
        Left err      -> putStrLn (Text.unpack err)
        Right payload -> run' payload



run' payload = do
    -- M.mapM_ PP.prettyPrint fns
    -- putStrLn $ Text.unpack (Display.renderFunctions functions)
    
    
    PP.prettyPrint (Payload.getModuleHeader payload)
    
    putStrLn (List.replicate 80 '-')
    
    putStrLn $ Text.unpack (Display.renderFunctions functions)
    
    -- M.mapM_ PP.prettyPrint functions
    
    -- alpha env functions
    -- beta functions
    
    
    where
        
        functions  = Payload.getFunctions payload
            -- |> map noMeta





