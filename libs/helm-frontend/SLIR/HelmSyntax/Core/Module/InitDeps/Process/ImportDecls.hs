{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module SLIR.HelmSyntax.Core.Module.InitDeps.Process.ImportDecls (
    processImports
) where


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

import qualified Data.List     as List
import qualified Data.Text     as Text
import qualified Data.Map      as Map
import qualified Data.Set      as Set
import qualified Data.Foldable as Fold
import qualified Data.Monoid   as Monoid
import qualified Data.Either   as Either

import qualified Data.Generics.Uniplate.Data as Uni
import qualified Text.Show.Prettyprint as PP


-- ~ HelmSyntax Cores

-- ~ HelmSyntax IR
import qualified SLIR.HelmSyntax.Data.Interface.Module.Payload as Payload
import qualified SLIR.HelmSyntax.Data.Initialization as Init

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

-- ~~ Header
import qualified SLIR.HelmSyntax.AST.Data.Header.Base       as Base
import qualified SLIR.HelmSyntax.AST.Data.Header.ImportDecl as Decl

--- Local
import SLIR.HelmSyntax.Core.Module.InitDeps.Data.MiscAliases (ForeignModule, ForeignNamespaceOverride)

-- ~
import qualified SLIR.HelmSyntax.Core.Module.InitDeps.Data.Report as Report
-- *




processImports :: [ForeignModule]
               -> Payload.Module
               -> Either Report.DependencyError Payload.Module

processImports foreignPayloads payload =
    case processImports' foreignPayloads payload of
        Left err -> Left err
        Right (fns, uns, fixs) ->
            Right $ Payload.initDependencies payload fns uns fixs




processImports' :: [ForeignModule]
               -> Payload.Module
               -> Either Report.DependencyError ([Decl.Function], [Decl.Union], [Decl.Infix])

processImports' foreignPayloads payload
    | null invalids =
        let
            (fns, uns, fixs) = List.unzip3 valids
        in
            Right (flatten fns, flatten uns, flatten fixs)
    
    | otherwise =
        Left $ List.head invalids

    where

        valids = Either.rights imports
        
        invalids = Either.lefts imports
        
        imports = map (processImport foreignPayloads) (Payload.getImports payload)






processImport :: [ForeignModule]
              -> Decl.ImportDecl
              -> Either Report.DependencyError ([Decl.Function], [Decl.Union], [Decl.Infix])

processImport foreignPayloads (isSudoFFINamespace -> True) =
    Right ([], [], [])


processImport foreignPayloads (Decl.ImportDecl name Nothing (Just Base.Everything))
    | Just foreignPayload <- getModule foreignPayloads name =
        let
            
            -- Initial Data
            fns  = Payload.getFunctions foreignPayload
                |> map (setOriginalNSFn name)

            uns  = Payload.getUnions    foreignPayload
                |> map (setOriginalNSUn name)

            fixs = Payload.getFixities  foreignPayload
                |> setOriginalNSGeneric name

        in
            Right (fns, uns, fixs)


processImport foreignPayloads (Decl.ImportDecl name Nothing (Just es))
    | Just foreignPayload <- getModule foreignPayloads name =
        let
            exports = Payload.getExports foreignPayload
            
            -- Initial Data
            fns  = Payload.getFunctions foreignPayload
                |> map (setOriginalNSFn name)
                |> mapMaybe (declEntriesFilter exports)
                |> mapMaybe (declEntriesFilter es)

            uns  = Payload.getUnions    foreignPayload
                |> map (setOriginalNSUn name)
                |> mapMaybe (unionEntriesFilter exports)
                |> mapMaybe (unionEntriesFilter es)

            fixs = Payload.getFixities  foreignPayload
                |> setOriginalNSGeneric name

        in
            Right (fns, uns, fixs)

processImport foreignPayloads (Decl.ImportDecl name asName optExplicits)
    | Just foreignPayload <- getModule foreignPayloads name =
        let
            -- Setup
            inNamespace = formatNSWithAsName name asName
            exports = Payload.getExports foreignPayload
            

            -- Initial Data
            fns  = Payload.getFunctions foreignPayload
                |> map (initFunction inNamespace)
                |> map (setOriginalNSFn name)
                |> mapMaybe (declEntriesFilter exports)
            
            uns  = Payload.getUnions foreignPayload
                |> map (initUnion inNamespace)
                |> map (setOriginalNSUn name)
                |> mapMaybe (unionEntriesFilter exports)
            
            fixs = Payload.getFixities foreignPayload
                |> setOriginalNSGeneric name
                |> map (initInfix inNamespace)

        in
            case optExplicits of
                Nothing ->
                    Right (fns, uns, fixs)
                
                -- TODO: Maybe also filter `Fixities`?
                Just es ->
                    Right
                        ( mapMaybe (declEntriesFilter es) fns
                        , mapMaybe (unionEntriesFilter es) uns
                        , fixs
                        )

    | otherwise =
        Left $ Report.ModuleImportNotFound name


-- formatNSWithExposing :: ID.Namespace -> Maybe Base.Entries -> ID.Namespace
-- formatNSWithExposing defaultNamespace Nothing = defaultNamespace
-- formatNSWithExposing defaultNamespace (Just Base.Everything)


formatNSWithAsName :: ID.Namespace -> Maybe ID.Big -> ID.Namespace
formatNSWithAsName defaultNamespace Nothing  = defaultNamespace

formatNSWithAsName defaultNamespace (Just (ID.Big asName _ _)) =
    ID.Namespace [asName]



getModule :: [ForeignModule] -> ID.Namespace -> Maybe ForeignModule
getModule foreignPayloads ns =
    List.find pred foreignPayloads
    where
        pred :: ForeignModule -> Bool
        pred x =
            Payload.getModuleName x == ns



-- *
-- | ## Entries Filter
-- *


declEntriesFilter :: Base.Entries -> Decl.Function -> Maybe Decl.Function
declEntriesFilter Base.Everything    fn = Just fn
declEntriesFilter (Base.Explicit es) fn
    | Just (Base.ValueEntry isSym txt) <- findEntry (getFnName fn) es' =
        Just fn
    
    | otherwise =
        Nothing

    where
        es' = mapMaybe getValueEntry es


unionEntriesFilter :: Base.Entries -> Decl.Union -> Maybe Decl.Union
unionEntriesFilter Base.Everything    un = Just un
unionEntriesFilter (Base.Explicit es) un
    | Just (Base.UnionEntry txt Nothing) <- findEntry (getUnName un) es' =
        Just un
    
    | Just (Base.UnionEntry txt (Just Base.UnionEverything)) <- findEntry (getUnName un) es' =
        Just un
    
    | Just (Base.UnionEntry txt (Just (Base.UnionExplicit conEntries))) <- findEntry (getUnName un) es' =
        Just
            $ constrEntriesFilter conEntries un
    
    | otherwise =
        Nothing

    where
        es' = mapMaybe getUnionEntry es


constrEntriesFilter :: [Text] -> Decl.Union -> Decl.Union
constrEntriesFilter es (Decl.Union name as cs m) =
    let
        cs' = mapMaybe (constrEntriesFilter' es) cs
    in
        Decl.Union name as cs' m


constrEntriesFilter' :: [Text] -> Decl.Constructor -> Maybe Decl.Constructor
constrEntriesFilter' segs c =
    case List.find entryPred segs of
        Nothing -> Nothing
        Just _  -> Just c

    where
        conName = getConName c
        
        entryPred :: Text -> Bool
        entryPred x =
            x == conName

--- *
--- | ### Entries Filter - Helpers
--- *

findEntry :: Text -> [Base.Entry] -> Maybe Base.Entry
findEntry name =
    List.find entryPred
    where
        entryPred :: Base.Entry -> Bool
        entryPred (Base.UnionEntry txt _) =
            txt == name
        
        -- NOTE: 'Maybe - TODO'?: Also check `isSym`?
        entryPred (Base.ValueEntry isSym' txt) =
            txt == name


findConEntry :: Text -> [Text] -> Maybe Text
findConEntry name =
    List.find entryPred
    where
        entryPred :: Text -> Bool
        entryPred txt =
            name == txt


getFnName :: Decl.Function -> Text
getFnName (Decl.FnDecl (ID.Low txt _ _) _ _ _ _) = txt
getFnName (Decl.OpDecl (ID.Sym txt _ _) _ _ _ _) = txt


getUnName :: Decl.Union -> Text
getUnName (Decl.Union (ID.Big txt _ _) _ _ _) = txt

getConName :: Decl.Constructor -> Text
getConName (Decl.Constructor (ID.Big txt _ _) _ _) = txt

isSym :: Decl.Function -> Bool
isSym Decl.OpDecl{} = True
isSym _             = False


getCons :: Decl.Union -> [Decl.Constructor]
getCons (Decl.Union _ _ cs _) = cs

getValueEntry :: Base.Entry -> Maybe Base.Entry
getValueEntry e@Base.ValueEntry{} = Just e
getValueEntry _                   = Nothing


getUnionEntry :: Base.Entry -> Maybe Base.Entry
getUnionEntry e@Base.UnionEntry{} = Just e
getUnionEntry _                   = Nothing




-- *
-- | Namespace Setters
-- *
initFunction :: ForeignNamespaceOverride -> Decl.Function -> Decl.Function
initFunction ns (Decl.FnDecl name args expr sig@(Just Etc.Validated{}) meta) =
    Decl.FnDecl
        (initLow ns name)
        args
        expr
        sig
        meta


initFunction ns (Decl.OpDecl sym args expr sig@(Just Etc.Validated{}) meta) =
    Decl.OpDecl
        (initSym ns sym)
        args
        expr
        sig
        meta


initFunction ns fn =
    error
        $ nonTCFunction ++ "\n" ++ PP.prettyShow fn


initUnion :: ForeignNamespaceOverride -> Decl.Union -> Decl.Union
initUnion ns (Decl.Union name as cons m) =
    Decl.Union
        (initBig ns name)
        as
        (map (initConstructor ns) cons)
        m


initConstructor :: ForeignNamespaceOverride -> Decl.Constructor -> Decl.Constructor
initConstructor ns (Decl.Constructor name ts m) =
    Decl.Constructor (initBig ns name) ts m




initInfix :: ForeignNamespaceOverride -> Decl.Infix -> Decl.Infix
initInfix ns (Decl.InfixL sum opPrec m) =
    Decl.InfixL
        (initSym ns sum)
        opPrec
        m

initInfix ns (Decl.InfixR sum opPrec m) =
    Decl.InfixR
        (initSym ns sum)
        opPrec
        m

initInfix ns (Decl.InfixN sum opPrec m) =
    Decl.InfixN
        (initSym ns sum)
        opPrec
        m




initSym :: ForeignNamespaceOverride -> ID.Sym -> ID.Sym
initSym ns (ID.Sym txt _ m) =
    ID.Sym txt (Just ns) m

initLow :: ForeignNamespaceOverride -> ID.Low -> ID.Low
initLow ns (ID.Low txt _ m) =
    ID.Low txt (Just ns) m

initBig :: ForeignNamespaceOverride -> ID.Big -> ID.Big
initBig ns (ID.Big txt _ m) =
    ID.Big txt (Just ns) m



setOriginalNSFn :: ID.Namespace
                -> Decl.Function
                -> Decl.Function

setOriginalNSFn ns (Decl.FnDecl name args expr sig meta) =
    Decl.FnDecl
        (setOriginalNSGeneric ns name)
        args
        expr
        sig
        (setOriginalNSGeneric ns meta)

setOriginalNSFn ns (Decl.OpDecl name args expr sig meta) =
    Decl.OpDecl
        (setOriginalNSGeneric ns name)
        args
        expr
        sig
        (setOriginalNSGeneric ns meta)



setOriginalNSUn :: ID.Namespace
                -> Decl.Union
                -> Decl.Union

setOriginalNSUn ns (Decl.Union name as cs meta) =
    Decl.Union
        (setOriginalNSGeneric ns name)
        as
        cs
        (setOriginalNSGeneric ns meta)




setOriginalNSGeneric :: (Data a, Typeable a) => ID.Namespace -> a -> a
setOriginalNSGeneric (ID.Namespace segs) =
    Uni.transformBi f
    where
        f :: Maybe Meta.Meta -> Maybe Meta.Meta
        f Nothing =
            Just $ Meta.Meta Nothing Nothing (Just segs)
        
        f (Just meta) =
            Just Meta.Meta
                { Meta.span = Meta.span meta
                , Meta.modulePath = Meta.modulePath meta
                , Meta.originalNamespace = Just segs
                }




-- *
-- | Misc. Error Messages
-- *


nonTCFunction =
       "Attempted dependency initialization of non-validated type scheme "
    ++ "(perhaps this function was never type checked!)…"




-- *
-- | Is Sudo FFI - Helpers
-- *

isSudoFFINamespace :: Decl.ImportDecl -> Bool
isSudoFFINamespace (Decl.ImportDecl (ID.Namespace ["Sudo", "Helm", "Native"]) _ _) = True
isSudoFFINamespace _ = False

