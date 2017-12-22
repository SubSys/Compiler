{-# LANGUAGE NoImplicitPrelude #-}
module HLIR.HelmIntro.Dev.Labs.Core.Parser where


-- *
import Core
import Core.Control.Flow
import Core.List.Util (flatten)

import Prelude (mapM_, IO, String)

import qualified Data.Text as Text


--- Dev
import qualified Dev.Samples.Basic      as BasicSamples
import qualified Dev.Samples.Complex    as ComplexSamples
import qualified Dev.Samples.TestParser as ParserSample

import qualified Text.Show.Prettyprint as PP

import qualified Text.Megaparsec.Pos        as Pos
import qualified Text.Megaparsec.Char       as C
import qualified Text.Megaparsec.Char.Lexer as L


-- Parser Framework
import Framework.Parser as FMP hiding (parse)


--- Local
import qualified HLIR.HelmIntro.Data.Interface.Module.Payload as Payload

import qualified HLIR.HelmIntro.Render.Utils as Display


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

-- ~ Parsers
import qualified HLIR.HelmIntro.Core.Module.Parser.Base.Types            as T
import qualified HLIR.HelmIntro.Core.Module.Parser.Base.Etc              as Etc
import qualified HLIR.HelmIntro.Core.Module.Parser.Base.Ident            as ID
import qualified HLIR.HelmIntro.Core.Module.Parser.TermLevel.Expressions as E
import qualified HLIR.HelmIntro.Core.Module.Parser.TopLevel.Unions       as Decl
import qualified HLIR.HelmIntro.Core.Module.Parser.TopLevel.Functions    as Decl
import qualified HLIR.HelmIntro.Core.Module.Parser.TopLevel.Fixities     as Decl


import qualified HLIR.HelmIntro.Core.Module.Parser.Driviver.Program as Program
import qualified HLIR.HelmIntro.Core.Module.Parser.Driviver.Header  as Header

import qualified HLIR.HelmIntro.Core.Module.Parser.Driver as ParserCore
-- *



{-# ANN module "HLint: ignore" #-}




run :: IO ()
run = do
    result <- ParserCore.parser ParserSample.sampleOne

    case result of
        Left err ->
            putStrLn err
        
        Right payload ->
            do
                putStrLn $ Text.unpack $ Display.renderFunctions functions
                -- putStrLn $ Text.unpack $ Display.renderUnions unions
                -- putStrLn "\n\n"
                
                -- mapM_ PP.prettyPrint functions
                
        
            where
                functions = Payload.getFunctions payload
                unions    = Payload.getUnions    payload





