{-# LANGUAGE NoImplicitPrelude #-}
module HLIR.HelmCore.Core.Uncurry.Dev where


-- *
import Core
import Core.Control.Flow ((|>), (<|))
import Prelude (return, String, IO, show, error)

import Data.List.Index  (imap)

import qualified Numeric.Natural as Natural

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

-- ~ HelmSyntax IR
import qualified HLIR.HelmCore.Data.Payload as Payload

--- Local Deps
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
import qualified HLIR.HelmCore.Pass.Uncurry.InsertImplicitArguments as ArgPass

-- ~ HelmCore Drivers
import qualified HLIR.HelmCore.Core.Indexing.Driver as Driver
import qualified HLIR.HelmCore.Core.TypeCheck.Driver as Driver
-- *



{-# ANN module "HLint: ignore" #-}





sample =
    ParserSample.sampleOne
        |> HelmSyntax.frontend
        |> HelmSyntax.toHelmCore
        |> Driver.globalize

run = do
    input <- sample
    
    case input of
        Left err      -> putStrLn (Text.unpack err)
        Right payload -> run' payload



run' payload = do
    
    putStrLn $ Text.unpack $ Display.renderFunctions functions'
    
    putStrLn "\n"
    
    case tc functions' of
        Left err -> PP.prettyPrint err
        Right fns ->
            putStrLn
                $ Text.unpack
                $ Display.renderFunctions fns
    
    where
        tc = typeCheck payload
        
        functions  = Payload.getFunctions payload
        
        functions' =
            map ArgPass.expandArguments functions


typeCheck payload fns =
    let payload' = Payload.updateFunctions payload  fns
        result = Driver.typeCheck' payload'
    in
        case result of
            Left err -> Left err
            Right payload ->
                Right $ Payload.getFunctions payload


