{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
module SLIR.HelmSyntax.Module.Core.TypeCheck.Driver (
    typeCheck
  , typeCheck'
  , typeCheckDebug
  , typeCheckDebug'
) where


-- *
import Core
import Core.Control.Flow ((|>), (<|))
import Core.List.Util    (flatten, singleton)
import Data.Monoid ((<>))
import Prelude
    ( return
    , String
    , IO
    , show
    , error
    , (<$>)
    , (>>=)
    , (>>)
    , fromIntegral
    )


import qualified Prelude    as Pre
import qualified Core.Utils as Core

import qualified Control.Monad              as M
import qualified Control.Monad.State        as M
import qualified Control.Monad.Except       as M
import qualified Control.Monad.RWS          as M
import qualified Control.Monad.Identity     as M
import qualified Control.Monad.Reader       as M
import qualified Control.Monad.Writer       as M
import qualified Control.Monad.Trans        as M

import qualified Data.List                    as List
import qualified Data.Text                    as Text
import qualified Data.Text.IO                 as TIO
import qualified Data.Map                     as Map
import qualified Data.Set                     as Set
import qualified Data.Foldable                as Fold
import qualified Data.Monoid                  as Monoid
import qualified Data.Maybe                   as Maybe
import qualified Data.Either                  as Either
import qualified Data.Char                    as Char
import qualified Data.Word                    as Word
import qualified Data.STRef                   as ST
import qualified Data.Bits                    as Bit
import qualified Data.Fixed                   as Fixed
import qualified Data.Vector.Unboxed          as V
import qualified Data.Vector.Unboxed.Mutable  as MV
import qualified Data.Vector.Generic          as VG
import qualified Data.IORef                   as IORef
import qualified Data.ByteString              as BS
import qualified Data.Functor                 as Fun
import qualified Data.Data                    as Data


-- + Recursion Schemes & Related
import qualified Data.Functor.Foldable       as F
import qualified Data.Generics.Uniplate.Data as Uni


-- + Dev & Debugging
import qualified Text.Show.Prettyprint as PP

-- + HelmSyntax Module Interface
import qualified SLIR.HelmSyntax.Module.Data.Interface as I

-- + HelmSyntax AST Utils
import qualified SLIR.HelmSyntax.AST.Utils.Scope           as Scope
import qualified SLIR.HelmSyntax.AST.Utils.Auxiliary.Ident as ID

-- + HelmSyntax AST
-- ++ Base
import qualified SLIR.HelmSyntax.AST.Data.Semantic.Base.Etc      as Etc
import qualified SLIR.HelmSyntax.AST.Data.Semantic.Base.Ident    as ID
import qualified SLIR.HelmSyntax.AST.Data.Semantic.Base.Types    as T
import qualified SLIR.HelmSyntax.AST.Data.Semantic.Base.Values   as V
import qualified SLIR.HelmSyntax.AST.Data.Semantic.Base.Metadata as Meta

-- ++ TermLevel
import qualified SLIR.HelmSyntax.AST.Data.Semantic.TermLevel.Expr     as E
import qualified SLIR.HelmSyntax.AST.Data.Semantic.TermLevel.Patterns as P

-- ++ TopLevel
import qualified SLIR.HelmSyntax.AST.Data.Semantic.TopLevel.Fixities  as Decl
import qualified SLIR.HelmSyntax.AST.Data.Semantic.TopLevel.Functions as Decl
import qualified SLIR.HelmSyntax.AST.Data.Semantic.TopLevel.Unions    as Decl


-- + Local
import qualified SLIR.HelmSyntax.Module.Core.TypeCheck.Data.Report                as Report
import qualified SLIR.HelmSyntax.Module.Core.TypeCheck.Inference.Data.Env         as Env
import qualified SLIR.HelmSyntax.Module.Core.TypeCheck.Solver.Data.Constraint     as Con
import qualified SLIR.HelmSyntax.Module.Core.TypeCheck.Inference.Init.Decl        as Init
import qualified SLIR.HelmSyntax.Module.Core.TypeCheck.Inference.Init.Unions      as Init
import qualified SLIR.HelmSyntax.Module.Core.TypeCheck.Inference.Engine           as Infer
import qualified SLIR.HelmSyntax.Module.Core.TypeCheck.Syntax.TopLevel.Functions  as Decl
-- *




{-# ANN module ("HLint: ignore" :: String) #-}





-- | Tmp...
--
type DebugFlag = Bool




typeCheck :: IO (Either Text I.Module) -> IO (Either Text I.Module)
typeCheck upstream = do
    result <- upstream
    
    case result of
        Left err -> return $ Left err
        Right payload ->
            return $ typeCheck' payload




typeCheck' :: I.Module -> Either Text I.Module
typeCheck' payload =
    let 
        -- Setups
        fns = I.getFunctions payload
        fnDeps = I.getFunctionDeps payload
        
        uns = I.getUnions payload
        unDeps = I.getUnionDeps payload
        
        
        
        -- Initial Data
        typesEnv =
            Map.unions
                [ initialEnv (unDeps ++ uns)
                , getTLTypes fnDeps
                ]
        
        overloads = 
            Init.initOverloads  (fnDeps ++ fns)

        -- Finish
        result =
            Infer.resolveDecls Decl.inferDecl (typesEnv, overloads) fns
        
    in
        case result of
            Left err ->
                Left
                    (formatError filePath moduleName err)

            Right (fns', _, _) ->
                Right $ I.updateFunctions payload fns'
    
    where
        -- Debugging Info
        moduleName = I.getModuleName payload
        filePath = I.getModulePath payload







-- *
-- | Internal
-- *


initialEnv :: [Decl.Union] -> Env.Types
initialEnv us =
    let unionTypes = map Init.genUnionSigs us
            |> flatten
            |> Map.fromList
    in
        unionTypes




-- | Get Top-Level (validated) Types
--

getTLTypes :: [Decl.Function] -> Env.Types
getTLTypes xs =
    Map.fromList $ map getTLType xs
    
    
    where
        getTLType (Decl.Function name _ _ (Decl.Validated scheme _) _) =
            (ID.get name, scheme)




-- | Generate Error Messages
-- (Something just barely good enough to be helpful..)


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
            , Text.pack "Some found span data:"
            , Text.pack "    " `Text.append` Text.pack (PP.prettyShow (getMetas err))
            ]



getMetas :: (Data.Data a, Data.Typeable a) => a -> [Meta.Span]
getMetas input =
    [x | (getSpan -> Just x) <- Uni.universeBi input]


getSpan :: Meta.Meta -> Maybe Meta.Span
getSpan x@Meta.Meta{} = Meta.span x
getSpan _ = Nothing




typeCheckDebug :: IO (Either Text I.Module) -> IO (Either Text I.Module)
typeCheckDebug upstream = do
    result <- upstream
    
    case result of
        Left err -> return $ Left err
        Right payload ->
            return $ typeCheckDebug' payload


typeCheckDebug' :: I.Module -> Either Text I.Module
typeCheckDebug' payload =
    let 
        -- Setups
        fns = I.getFunctions payload
        fnDeps = I.getFunctionDeps payload
        
        uns = I.getUnions payload
        unDeps = I.getUnionDeps payload
        
        
        
        -- Initial Data
        typesEnv =
            Map.unions
                [ initialEnv (unDeps ++ uns)
                , getTLTypes fnDeps
                ]
        
        overloads = 
            Init.initOverloads  (fnDeps ++ fns)

        -- Finish
        result =
            Infer.resolveDecls Decl.inferDecl (typesEnv, overloads) fns
        
    in
        case result of
            Left err ->
                Left
                    (formatError filePath moduleName err)
        
            Right (fns', _, _) ->
                Right $ I.updateFunctions payload fns'
    
    where
        -- Debugging Info
        moduleName = I.getModuleName payload
        filePath = I.getModulePath payload


