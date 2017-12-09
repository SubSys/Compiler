{-# LANGUAGE NoImplicitPrelude #-}
module HLIR.HelmOutro.Core.TypeCheck.Driver (
      typeCheck
    , typeCheck'
    , initialEnv
) where


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
import qualified Text.Show.Prettyprint as PP


--- Local Deps
-- ~ HelmOutro IR
import qualified HLIR.HelmOutro.Data.Payload as Payload

-- ~ HelmOutro AST
-- ~~ Base
import qualified HLIR.HelmOutro.AST.Base.Ident  as ID
import qualified HLIR.HelmOutro.AST.Base.Types  as T
import qualified HLIR.HelmOutro.AST.Base.Values as V
import qualified HLIR.HelmOutro.AST.Base.Etc    as Etc
-- ~~ TermLevel
import qualified HLIR.HelmOutro.AST.TermLevel.Expressions as E
import qualified HLIR.HelmOutro.AST.TermLevel.Patterns    as P
-- ~~ TopLevel
import qualified HLIR.HelmOutro.AST.TopLevel.Functions as Decl
import qualified HLIR.HelmOutro.AST.TopLevel.Unions    as Decl

--- Local
import qualified HLIR.HelmOutro.Core.TypeCheck.Data.Env                    as Env
import qualified HLIR.HelmOutro.Core.TypeCheck.Data.Report                 as Report
import qualified HLIR.HelmOutro.Core.TypeCheck.Data.Subst                  as Sub
import qualified HLIR.HelmOutro.Core.TypeCheck.Data.System                 as Sys
import qualified HLIR.HelmOutro.Core.TypeCheck.Data.TypeSystem             as TS
import qualified HLIR.HelmOutro.Core.TypeCheck.Data.Canonical.Ident        as CID
import qualified HLIR.HelmOutro.Core.TypeCheck.Data.System.Constraints     as Con
import qualified HLIR.HelmOutro.Core.TypeCheck.Data.System.Scope           as Scope


-- ~ Infer Decls
import qualified HLIR.HelmOutro.Core.TypeCheck.Syntax.Decl as Decl

-- ~ Init Stuff
import qualified HLIR.HelmOutro.Core.TypeCheck.Init.Unions as Union

-- ~ Finish
import qualified HLIR.HelmOutro.Core.TypeCheck.Resolve as Resolve

-- ~ Most critical! Ensure indices are globally unique.
import HLIR.HelmOutro.Core.Indexing.Driver.Globalize (globalize)
-- *






typeCheck :: IO (Either Text Payload.Module) -> IO (Either Text Payload.Module)
typeCheck input = do
    result <- input
    case result of
        Left err -> return $ Left err
        Right payload ->
            return $ typeCheck' payload




typeCheck' ::  Payload.Module -> Either Text Payload.Module
typeCheck' payload =
    let -- Initial Data
        env = initialEnv $ Payload.getUnions payload
        fns = globalize $ Payload.getFunctions payload
        
        -- Finish
        result = Resolve.resolveDecls Decl.inferDecl env fns
        
    in
        case result of
            Left err ->
                Left $ Text.pack
                     $ PP.prettyShow err

            Right (fns', _, _) ->
                Right $ Payload.updateFunctions payload fns'




-- *
-- | Internal
-- *


initialEnv :: [Decl.Union] -> Env.Env
initialEnv us =
    let unionTypes = map Union.genUnionSigs us
            |> flatten
            |> Map.fromList
    in
        Env.Env
            { Env.types = unionTypes
            }
















