{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
module SLIR.HelmSyntax.Dev.Labs.Misc.Beta where


-- *
import Core
-- import Core.Control.Flow
import Core.List.Util (flatten)
import Data.Dynamic (Dynamic)
import Data.Data (Data, Typeable)
import Prelude (mapM_, IO, String, return)
import GHC.Types (Constraint)

import qualified Data.Text as Text
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Generics.Uniplate.Data as Uni
import qualified Control.Monad.State.Lazy as State




--- Dev
import qualified Dev.Samples.Basic      as BasicSamples
import qualified Dev.Samples.Complex    as ComplexSamples
import qualified Dev.Samples.TestParser as ParserSample

import qualified Text.Show.Prettyprint as PP


--- Local
import qualified SLIR.HelmSyntax.Data.Interface.Module.Payload as Payload
import qualified SLIR.HelmSyntax.Render.Utils as Display


-- ~ HelmSyntax AST Utils
import qualified SLIR.HelmSyntax.AST.Toolbox.Format.NoMeta as Format


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
import qualified SLIR.HelmSyntax.Core.Module.Parser.Base.Types            as T
import qualified SLIR.HelmSyntax.Core.Module.Parser.Base.Etc              as Etc
import qualified SLIR.HelmSyntax.Core.Module.Parser.Base.Ident            as ID
import qualified SLIR.HelmSyntax.Core.Module.Parser.TermLevel.Expressions as E
import qualified SLIR.HelmSyntax.Core.Module.Parser.TopLevel.Unions       as Decl
import qualified SLIR.HelmSyntax.Core.Module.Parser.TopLevel.Functions    as Decl
import qualified SLIR.HelmSyntax.Core.Module.Parser.TopLevel.Fixities     as Decl


import qualified SLIR.HelmSyntax.Core.Module.Parser.Driviver.Program as Program
import qualified SLIR.HelmSyntax.Core.Module.Parser.Driviver.Header  as Header

import qualified SLIR.HelmSyntax.Core.Module.Parser.Driver as ParserCore
-- *



{-# ANN module "HLint: ignore" #-}




run :: IO ()
run = do
    result <- ParserCore.parser ParserSample.sampleOne

    case result of
        Left err ->
            putStrLn err
        
        Right payload ->
            -- mapM_ PP.prettyPrint functions
            
            
            run' functions
            
            -- mapM_ PP.prettyPrint (IR.binder 0 functions)
            
            -- do
            --     putStrLn $ Text.unpack $ Display.renderFunctions functions
            --     putStrLn $ Text.unpack $ Display.renderUnions unions
            --     putStrLn "\n\n"
        
            where
                functions = Payload.getFunctions payload
                unions    = Payload.getUnions    payload



-- 
-- 
-- instance Show Showable
--   where
--   showsPrec p (MkShowable a) = showsPrec p a

run' decls = do
    -- putStrLn $ Text.unpack $ Display.renderFunctions (map Format.noMeta decls)
    -- putStrLn "\n\n"
    putStrLn $ Text.unpack $ Display.renderFunctions decls





