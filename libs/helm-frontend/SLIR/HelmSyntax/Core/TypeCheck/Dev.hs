{-# LANGUAGE NoImplicitPrelude #-}
module SLIR.HelmSyntax.Core.TypeCheck.Dev where


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
import qualified SLIR.HelmSyntax.AST.Data.Base.Metadata as Meta

-- ~~ TermLevel
import qualified SLIR.HelmSyntax.AST.Data.TermLevel.Expressions as E
import qualified SLIR.HelmSyntax.AST.Data.TermLevel.Patterns    as P

-- ~~ TopLevel
import qualified SLIR.HelmSyntax.AST.Data.TopLevel.Fixities  as Decl
import qualified SLIR.HelmSyntax.AST.Data.TopLevel.Functions as Decl
import qualified SLIR.HelmSyntax.AST.Data.TopLevel.Unions    as Decl

--- Local
import qualified SLIR.HelmSyntax.Core.TypeCheck.Data.Env                    as Env
import qualified SLIR.HelmSyntax.Core.TypeCheck.Data.Report                 as Report
import qualified SLIR.HelmSyntax.Core.TypeCheck.Data.Subst                  as Sub
import qualified SLIR.HelmSyntax.Core.TypeCheck.Data.System                 as Sys
import qualified SLIR.HelmSyntax.Core.TypeCheck.Data.TypeSystem             as TS
import qualified SLIR.HelmSyntax.Core.TypeCheck.Data.Canonical.Ident        as CID
import qualified SLIR.HelmSyntax.Core.TypeCheck.Data.System.Constraints     as Con
import qualified SLIR.HelmSyntax.Core.TypeCheck.Data.System.Scope           as Scope
import qualified SLIR.HelmSyntax.Core.TypeCheck.Data.Unification.Solver     as Solver

-- ~ Infer Decls
import qualified SLIR.HelmSyntax.Core.TypeCheck.Syntax.Decl as Decl

-- ~ Init Stuff
import qualified SLIR.HelmSyntax.Core.TypeCheck.Init.Unions as Union

-- ~ Finish
import qualified SLIR.HelmSyntax.Core.TypeCheck.Resolve as Resolve
-- *

{-# ANN module ("HLint: ignore" :: String) #-}




sample =
    ParserSample.sampleOne
        |> Driver.parser

run = do
    input <- sample
    
    case input of
        Left err      -> putStrLn (Text.unpack err)
        Right payload -> run' payload



run' payload =
    -- M.mapM_ PP.prettyPrint fns
    -- putStrLn $ Text.unpack (Display.renderFunctions functions)
    
    -- putStrLn $ Text.unpack (Display.renderFunctions functions)
    
    -- M.mapM_ PP.prettyPrint functions
    
    alpha env functions
    
    
    where
        env = initialEnv (Payload.getUnions payload)
        
        functions  = Payload.getFunctions payload
            -- |> map noMeta



noMeta x =
    Uni.transformBi f x
    where
        f :: Maybe Meta.Meta -> Maybe Meta.Meta
        f x = Nothing


initialEnv :: [Decl.Union] -> Env.Env
initialEnv us =
    let unionTypes = map Union.genUnionSigs us
            |> flatten
            |> Map.fromList
    in
        Env.Env
            { Env.types = unionTypes
            }




-- *
-- | Resolve Infered Syntax
-- *


alpha env fns =
    case tc of
        Left err -> PP.prettyPrint err
        Right (fns', env, cs) ->
            putStrLn $ Text.unpack (Display.renderFunctions fns')
            -- PP.prettyPrint env
    
    where
        tc = Resolve.resolveDecls Decl.inferDecl env fns





