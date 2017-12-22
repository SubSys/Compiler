{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module HLIR.HelmIntro.Dev.Labs.Misc.Alpha where


-- *
import Core
import Core.Control.Flow
import Core.List.Util (flatten)
import Data.Dynamic (Dynamic)
import Data.Data (Data, Typeable)
import Prelude (mapM_, IO, String, return, (<$>))
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
import qualified HLIR.HelmIntro.Data.Interface.Module.Payload as Payload
import qualified HLIR.HelmIntro.Render.Utils as Display


-- ~ HelmSyntax AST Utils
import qualified HLIR.HelmIntro.AST.Toolbox.Format.NoMeta as Format


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

-- ~ HelmSyntax AST Toolbox
-- ~~ Globalize
-- import HLIR.HelmIntro.AST.Toolbox.Internal.Globalize.Indexer as Index

-- ~ TMP!
import qualified HLIR.HelmIntro.Internal.AST as IR

-- ~~ Indexing
import HLIR.HelmIntro.Internal.AST.Indexing.Data.Env (Env)
import HLIR.HelmIntro.Internal.AST.Indexing.Classes (Bindable(..), Indexable(..), bind, index)

import qualified HLIR.HelmIntro.Internal.AST.Indexing.Data.Env as Env
import HLIR.HelmIntro.Internal.AST.Indexing.Instances ()
import HLIR.HelmIntro.Internal.AST.Instances.Essential ()

import HLIR.HelmIntro.Internal.AST.Instances.StripMeta ()
import HLIR.HelmIntro.Internal.AST.Classes.StripMeta (StripMeta(..), stripMeta)
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





run' decls =
    let result = alpha decls
    in
        -- PP.prettyPrint $ List.head decls
        putStrLn $ Text.unpack $ Display.renderFunctions result



alpha :: [Decl.Function] -> [Decl.Function]
alpha xs =
    let (ys, s) = State.runState (index xs) Env.initEnv
    in
        ys
        -- xs






