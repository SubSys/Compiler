{-# LANGUAGE NoImplicitPrelude #-}
module SLIR.HelmSyntax.Core.Module.TypeCheck.Driver (
      typeCheck
    , typeCheck'
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
import qualified Data.String   as String

import qualified Data.Generics.Uniplate.Data as Uni
import qualified Text.Show.Prettyprint as PP


--- Local Deps
-- ~ HelmSyntax IR
import qualified SLIR.HelmSyntax.Data.Interface.Module.Payload as Payload

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

--- Local
import qualified SLIR.HelmSyntax.Core.Module.TypeCheck.Data.Interface.TypesEnv     as TI
import qualified SLIR.HelmSyntax.Core.Module.TypeCheck.Data.Report                 as Report
import qualified SLIR.HelmSyntax.Core.Module.TypeCheck.Data.Subst                  as Sub
import qualified SLIR.HelmSyntax.Core.Module.TypeCheck.Data.System                 as Sys
import qualified SLIR.HelmSyntax.Core.Module.TypeCheck.Data.TypeSystem             as TS
import qualified SLIR.HelmSyntax.AST.Auxiliary.Canonical.Ident              as CID
import qualified SLIR.HelmSyntax.Core.Module.TypeCheck.Data.System.Constraints     as Con
import qualified SLIR.HelmSyntax.Core.Module.TypeCheck.Data.System.Scope           as Scope


-- ~ Infer Decls
import qualified SLIR.HelmSyntax.Core.Module.TypeCheck.Syntax.Decl as Decl

-- ~ Init Stuff
import qualified SLIR.HelmSyntax.Core.Module.TypeCheck.Init.Unions as Union
import qualified SLIR.HelmSyntax.Core.Module.TypeCheck.Init.Overloads as Overloads

-- ~ Finish
import qualified SLIR.HelmSyntax.Core.Module.TypeCheck.Resolve as Resolve
-- *



typeCheck :: IO (Either Text Payload.Module) -> IO (Either Text Payload.Module)
typeCheck upstream = do
    result <- upstream
    
    case result of
        Left err -> return $ Left err
        Right payload ->
            return $ typeCheck' payload




typeCheck' :: Payload.Module -> Either Text Payload.Module
typeCheck' payload =
    let 
        -- Setups
        fns = Payload.getFunctions payload
        fnDeps = Payload.getFunctionDeps payload
        
        uns = Payload.getUnions payload
        unDeps = Payload.getUnionDeps payload
        
        
        
        -- Initial Data
        typesEnv =
            TI.merge
                (initialEnv (unDeps ++ uns))
                (getTLTypes fnDeps)
        
        overloads = 
            Overloads.initOverloads  (fnDeps ++ fns)

        -- Finish
        result =
            Resolve.resolveDecls Decl.inferDecl (typesEnv, overloads) fns
        
    in
        case result of
            Left err ->
                Left
                    (formatError filePath moduleName err)

            Right (fns', _, _) ->
                Right $ Payload.updateFunctions payload fns'
    
    where
        -- Debugging Info
        moduleName = Payload.getModuleName payload
        filePath = Payload.getModulePath payload



formatError :: Text -> ID.Namespace -> Report.TypeError -> Text
formatError filePath (ID.Namespace segs) err =
    let
        errText = Text.pack $ PP.prettyShow err
        nsText = Text.intercalate (Text.pack ".") segs
    in
        Text.unlines
            [ errText
            , Text.pack "filePath: " `Text.append` filePath
            , Text.pack "moduelName: " `Text.append` nsText
            ]




-- *
-- | Internal
-- *


initialEnv :: [Decl.Union] -> TI.Env
initialEnv us =
    let unionTypes = map Union.genUnionSigs us
            |> flatten
            |> Map.fromList
    in
        TI.Env
            { TI.types = unionTypes
            }




-- | Get Top-Level (validated) Types
--

getTLTypes :: [Decl.Function] -> TI.Env
getTLTypes xs =
    map getTLType xs
        |> TI.mergeEnvs

getTLType :: Decl.Function -> TI.Env
getTLType (Decl.FnDecl name _ _ (Just (Etc.Validated scheme _)) _) =
    TI.singleton (CID.ident name) scheme

getTLType (Decl.OpDecl name _ _ (Just (Etc.Validated scheme _)) _) =
    TI.singleton (CID.ident name) scheme










