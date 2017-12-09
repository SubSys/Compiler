{-# LANGUAGE NoImplicitPrelude #-}
module SLIR.HelmSyntax.Dev.Labs.DryRun where


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

import qualified Data.Generics.Uniplate.Data as Uni


--- Local
import qualified SLIR.HelmSyntax.Data.Payload as Payload

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

-- ~ Parsers
import qualified SLIR.HelmSyntax.Core.Parser.Base.Types            as T
import qualified SLIR.HelmSyntax.Core.Parser.Base.Etc              as Etc
import qualified SLIR.HelmSyntax.Core.Parser.Base.Ident            as ID
import qualified SLIR.HelmSyntax.Core.Parser.TermLevel.Expressions as E
import qualified SLIR.HelmSyntax.Core.Parser.TopLevel.Unions       as Decl
import qualified SLIR.HelmSyntax.Core.Parser.TopLevel.Functions    as Decl
import qualified SLIR.HelmSyntax.Core.Parser.TopLevel.Fixities     as Decl


import qualified SLIR.HelmSyntax.Core.Parser.Driviver.Program as Program
import qualified SLIR.HelmSyntax.Core.Parser.Driviver.Header  as Header

import qualified SLIR.HelmSyntax.Core.Parser.Driviver as ParserCore
-- *



{-# ANN module "HLint: ignore" #-}




run :: IO ()
run = do
    result <- ParserCore.parser ParserSample.sampleOne

    case result of
        Left err ->
            putStrLn err
        
        Right payload ->
            PP.prettyPrint payload



-- 
-- process :: 
-- process =
--     Uni.transform f
--     where
--         f x = x



