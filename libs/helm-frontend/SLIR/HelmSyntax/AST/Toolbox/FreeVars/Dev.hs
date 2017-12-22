{-# LANGUAGE NoImplicitPrelude #-}
module SLIR.HelmSyntax.AST.Toolbox.FreeVars.Dev where


-- *
import Core
import Core.Control.Flow ((|>), (<|))
import Core.List.Util    (flatten)
import Data.Data (Data, Typeable)

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


--- Dev
import qualified Dev.Samples.Basic      as BasicSamples
import qualified Dev.Samples.Complex    as ComplexSamples
import qualified Dev.Samples.TestParser as ParserSample

import qualified SLIR.HelmSyntax.Render.Utils as Display

-- ~ HelmSyntax Cores
import qualified SLIR.HelmSyntax.Core.Module.Parser.Driver    as Driver
import qualified SLIR.HelmSyntax.Core.Module.TypeCheck.Driver as Driver
import qualified SLIR.HelmSyntax.Core.Module.InitDeps.Driver  as Driver
import qualified SLIR.HelmSyntax.Core.Module.Normalize.Driver as Driver

import qualified SLIR.HelmSyntax.Data.Interface.Utils as IUtils

-- ~ HelmIntro 
import qualified SLIR.HelmSyntax.Feed.HelmIntro as HelmIntro

-- ~ HelmSyntax IR
import qualified SLIR.HelmSyntax.Data.Interface.Program.Payload as Payload

--- Local Deps
-- ~ HelmSyntax AST
-- ~~ Base
import qualified SLIR.HelmSyntax.AST.Data.Base.Etc      as Etc
import qualified SLIR.HelmSyntax.AST.Data.Base.Ident    as ID
import qualified SLIR.HelmSyntax.AST.Data.Base.Types    as T
import qualified SLIR.HelmSyntax.AST.Data.Base.Values   as V
import qualified SLIR.HelmSyntax.AST.Data.Base.Metadata as Meta

-- ~~ TermLevel
import qualified SLIR.HelmSyntax.AST.Data.TermLevel.Expressions as E
import qualified SLIR.HelmSyntax.AST.Data.TermLevel.Patterns    as P

-- ~~ TopLevel
import qualified SLIR.HelmSyntax.AST.Data.TopLevel.Fixities  as Decl
import qualified SLIR.HelmSyntax.AST.Data.TopLevel.Functions as Decl
import qualified SLIR.HelmSyntax.AST.Data.TopLevel.Unions    as Decl

-- ~~ Auxiliary AST - Nodes & Utils
import qualified SLIR.HelmSyntax.AST.Auxiliary.Canonical.Ident            as CID

--- Local
import qualified SLIR.HelmSyntax.AST.Toolbox.FreeVars.Toolkit as Toolkit
-- *




{-# ANN module "HLint: ignore" #-}






upstream =
    let filePath   = Text.pack ParserSample.sampleOnePath
        sourceCode = ParserSample.sampleOne
    in
        sourceCode
            |> Driver.runModuleParser filePath
            |> Driver.typeCheck
            |> Driver.initDeps []
            |> Driver.normalize



run = do
    result <- upstream
    
    case result of
        Left err -> PP.prettyPrint err
        Right payload ->
            run' (IUtils.aggregateModules [payload])



run' paylaod = do
    -- putStrLn
    --     $ Text.unpack
    --     $ Display.renderFunctions (Payload.getFunctions paylaod)

    M.mapM_ format fns
    
    where
        format fn = do
            putStrLn (List.replicate 100 '*')
            putStrLn "\n\n\n"
            
            PP.prettyPrint (noMeta $ CID.ident fn)
            putStrLn (List.replicate 90 '-')
            PP.prettyPrint (noMeta $ Toolkit.query fn)
            
            putStrLn "\n\n\n"
            putStrLn (List.replicate 100 '*')
            
            
        
        fns = noMeta $ Payload.getFunctions paylaod





noMeta :: (Data a, Typeable a) => a -> a
noMeta =
    Uni.transformBi f
    where
        f :: Maybe Meta.Meta -> Maybe Meta.Meta
        f _ = Nothing



