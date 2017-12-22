{-# LANGUAGE NoImplicitPrelude #-}
module HLIR.HelmIntro.Dev.Labs.Core.Globalize where


-- *
import Core
import Core.Control.Flow hiding (apply)
import Core.List.Util (flatten)

import Prelude (mapM_, IO, String, Monoid, (>>=), return, (!!), error, lookup, show, (<$>))

import Data.Monoid as Monoid

import qualified Data.List  as List
import qualified Data.Text as Text
import qualified Data.Map  as Map
import qualified Data.Set  as Set

import qualified Data.Foldable as Fold

import Control.Monad.State    as M
import Control.Monad.Except   as M
import Control.Monad.RWS      as M
import Control.Monad.Identity as M
import Control.Monad.Reader   as M

import qualified Data.Generics.Uniplate.Data as Uni


--- Dev
import qualified Dev.Samples.Basic      as BasicSamples
import qualified Dev.Samples.Complex    as ComplexSamples
import qualified Dev.Samples.TestParser as ParserSample

import qualified Text.Show.Prettyprint as PP


--- Local
import qualified HLIR.HelmIntro.Render.Utils as Display

-- ~ HelmSyntax IR
import qualified HLIR.HelmIntro.Data.Interface.Module.Payload as Payload

-- ~ HelmSyntax AST
-- ~~ Base
import qualified HLIR.HelmIntro.AST.Data.Base.Etc    as Etc
import qualified HLIR.HelmIntro.AST.Data.Base.Ident  as ID
import qualified HLIR.HelmIntro.AST.Data.Base.Types  as T
import qualified HLIR.HelmIntro.AST.Data.Base.Values as V

-- ~~ TermLevel
import qualified HLIR.HelmIntro.AST.Data.TermLevel.Expressions as E
import qualified HLIR.HelmIntro.AST.Data.TermLevel.Patterns    as P

-- ~~ TopLevel
import qualified HLIR.HelmIntro.AST.Data.TopLevel.Fixities  as Decl
import qualified HLIR.HelmIntro.AST.Data.TopLevel.Functions as Decl
import qualified HLIR.HelmIntro.AST.Data.TopLevel.Unions    as Decl


-- ~ HelmSyntax Cores
import qualified HLIR.HelmIntro.Core.Module.Parser.Driver as ParserCore
import qualified HLIR.HelmIntro.Core.TypeCheck.Driver as TypeCheckCore


-- ~ HelmSyntax Passes
import qualified HLIR.HelmIntro.Pass.Standard.Initial.Rename.Locals as RenameLocalsPass
-- *



{-# ANN module ("HLint: ignore" :: String) #-}



run :: IO ()
run = do
    result <- ParserCore.parser ParserSample.sampleOne

    case result of
        Left err ->
            putStrLn err
        
        Right parsed -> do
            -- run' parsed
            run' $ RenameLocalsPass.process' parsed


run' input = do
    putStrLn $ Text.unpack $ Display.renderFunctions functions
    putStrLn $ Text.unpack $ Display.renderUnions unions
    putStrLn "\n\n"


    where
        functions = Payload.getFunctions input
        unions    = Payload.getUnions    input


