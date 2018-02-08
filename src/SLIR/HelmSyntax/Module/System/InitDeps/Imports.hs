{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternGuards #-}
module SLIR.HelmSyntax.Module.System.InitDeps.Imports (
    processImports
  , ForeignModule
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
import qualified SLIR.HelmSyntax.AST.Utils.Scope                       as Scope
import qualified SLIR.HelmSyntax.AST.Utils.Auxiliary.Ident             as ID
import qualified SLIR.HelmSyntax.AST.Utils.Auxiliary.Functions.SudoFFI as SudoFFI

-- + HelmSyntax AST
-- ++ Base
import qualified SLIR.HelmSyntax.AST.Data.Semantic.Base.Etc      as Etc
import qualified SLIR.HelmSyntax.AST.Data.Semantic.Base.Ident    as ID
import qualified SLIR.HelmSyntax.AST.Data.Semantic.Base.Types    as T
import qualified SLIR.HelmSyntax.AST.Data.Semantic.Base.Values   as V
import qualified SLIR.HelmSyntax.AST.Data.Semantic.Base.Metadata as Meta
import qualified SLIR.HelmSyntax.AST.Data.Semantic.Base.Header   as Header

-- ++ TermLevel
import qualified SLIR.HelmSyntax.AST.Data.Semantic.TermLevel.Expr     as E
import qualified SLIR.HelmSyntax.AST.Data.Semantic.TermLevel.Patterns as P

-- ++ TopLevel
import qualified SLIR.HelmSyntax.AST.Data.Semantic.TopLevel.Fixities  as Decl
import qualified SLIR.HelmSyntax.AST.Data.Semantic.TopLevel.Functions as Decl
import qualified SLIR.HelmSyntax.AST.Data.Semantic.TopLevel.Unions    as Decl


-- + Local
import qualified SLIR.HelmSyntax.Module.System.InitDeps.Data.Report as Report
-- *




{-# ANN module ("HLint: ignore" :: String) #-}





type ForeignModule = I.Module




processImports :: [ForeignModule]
                -> I.Module
                -> Either
                    Report.DependencyError
                    I.Module

processImports mods payload
    | not (null errors) =
        Left (Report.DependencyErrors errors)
    
    | otherwise =
        let
            (fns, uns, fxs) = List.unzip3 valids
        in
            Right
                $ I.initDependencies payload (flatten fns) (flatten uns) (flatten fxs)


    where
        errors = Either.lefts deps
        valids = Either.rights deps
        deps = map (processImport mods) (I.getImports payload)


processImport :: [ForeignModule]
              -> Header.ImportDecl
              -> Either
                    Report.DependencyError
                    ([Decl.Function], [Decl.Union], [Decl.Infix])
processImport _ (Header.ImportDecl (SudoFFI.isSudoNS' -> True) _ _) = Right ([], [], [])

processImport mods (Header.ImportDecl name Nothing (Just Header.Everything))
    | Just payload <- getModule name mods =
        let
            -- Finalize
            fns = I.getFunctions payload
                |> recordOriginalDeclNS name
            uns = I.getUnions payload
                |> recordOriginalUnionNS name
            fxs = I.getFixities payload
                |> recordOriginalNamespace name
        in
            Right (fns, uns, fxs)

processImport mods (Header.ImportDecl name asName (Just entries))
    | Just payload <- getModule name mods =
        let
            -- Setup
            exports = I.getExports payload
            namespace = formatNSWithAsName name asName

            -- Finalize
            fns = I.getFunctions payload
                |> declEntriesFilter exports
                |> declEntriesFilter entries
                |> initFuns namespace
                |> recordOriginalDeclNS name

            uns = I.getUnions payload
                |> unionEntriesFilter exports
                |> unionEntriesFilter entries
                |> initUnions namespace
                |> recordOriginalUnionNS name

            fxs = I.getFixities payload
                |> namespaceOverride namespace
                |> recordOriginalNamespace name

        in
            Right (fns, uns, fxs)



processImport _ (Header.ImportDecl name _ _) =
    Left $ Report.ModuleImportNotFound name


-- | Setup/Namespace Injection 
-- 

initFuns :: ID.Namespace -> [Decl.Function] -> [Decl.Function]
initFuns ns = map initFun'
    where
        initFun' (Decl.Function name args expr sig meta) =
            Decl.Function
                (namespaceOverride ns name)
                args
                expr
                sig
                meta

initUnions :: ID.Namespace -> [Decl.Union] -> [Decl.Union]
initUnions ns = map initUnion'
    where
        initConstr (Decl.Constructor name args meta) =
            Decl.Constructor
                (namespaceOverride ns name)
                args
                meta

        initUnion' (Decl.Union name as cs meta) =
            Decl.Union
                (namespaceOverride ns name)
                as
                (map initConstr cs)
                meta


namespaceOverride :: (Data.Data a, Data.Typeable a) => ID.Namespace -> a -> a
namespaceOverride ns = Uni.transformBi f
    where
        f :: Maybe ID.Namespace -> Maybe ID.Namespace
        f _ = Just ns



-- | Entries Filter
--

declEntriesFilter :: Header.Entries -> [Decl.Function] -> [Decl.Function]
declEntriesFilter Header.Everything fns         = fns
declEntriesFilter (Header.Explicit entries) fns =
    mapMaybe (declEntriesFilter' entries) fns

declEntriesFilter' :: [Header.Entry] -> Decl.Function -> Maybe Decl.Function
declEntriesFilter' es fn
    | Just Header.ValueEntry{} <- findEntry (ID.get fn) es =
        Just fn
    
    | otherwise =
        Nothing





unionEntriesFilter :: Header.Entries -> [Decl.Union] -> [Decl.Union]
unionEntriesFilter Header.Everything decls         = decls
unionEntriesFilter (Header.Explicit entries) decls =
    mapMaybe (unionEntriesFilter' entries) decls

unionEntriesFilter' :: [Header.Entry] -> Decl.Union -> Maybe Decl.Union
unionEntriesFilter' es decl
    | Just (Header.UnionEntry txt Nothing) <- findEntry (ID.get decl) es =
        Just decl
    
    | Just (Header.UnionEntry txt (Just Header.UnionEverything)) <- findEntry (ID.get decl) es =
        Just decl
    
    | Just (Header.UnionEntry txt (Just (Header.UnionExplicit cons))) <- findEntry (ID.get decl) es =
        Just $ constrEntriesFilter cons decl
    
    | otherwise =
        Nothing



constrEntriesFilter :: [Text] -> Decl.Union -> Decl.Union
constrEntriesFilter es (Decl.Union name as cs meta) =
    let
        cs' = mapMaybe (constrEntriesFilter' es) cs
    in
        Decl.Union name as cs' meta



constrEntriesFilter' :: [Text] -> Decl.Constructor -> Maybe Decl.Constructor
constrEntriesFilter' es constr@((ID.getText . ID.get) -> name) =
    case List.find entryPred es of
        Just _  -> Just constr
        Nothing -> Nothing
    
    where
        entryPred x =
            x == name


-- | Entries Filter - Helpers
--

findEntry :: ID.Ident -> [Header.Entry] -> Maybe Header.Entry
findEntry (ID.getText -> name) = List.find entryPred
    where
        entryPred :: Header.Entry -> Bool
        entryPred (Header.ValueEntry _ txt) = name == txt
        entryPred (Header.UnionEntry txt _) = name == txt





getModule :: ID.Namespace -> [ForeignModule] -> Maybe ForeignModule
getModule ns = List.find modulePred
    where
        modulePred :: ForeignModule -> Bool
        modulePred (I.getModuleName -> name) =
            name == ns


formatNSWithAsName :: ID.Namespace -> Maybe ID.Ident -> ID.Namespace
formatNSWithAsName ns Nothing = ns
formatNSWithAsName _ (Just (ID.getText -> name)) =
    ID.Namespace [name]


recordOriginalDeclNS :: ID.Namespace -> [Decl.Function] -> [Decl.Function]
recordOriginalDeclNS ns = map f
    where
        f :: Decl.Function -> Decl.Function
        f (Decl.Function name args expr sig meta) =
            Decl.Function
                (recordOriginalNamespace ns name)
                args
                expr
                sig
                (recordOriginalNamespace ns meta)


recordOriginalUnionNS :: ID.Namespace -> [Decl.Union] -> [Decl.Union]
recordOriginalUnionNS ns = map union
    where
        union :: Decl.Union -> Decl.Union
        union (Decl.Union name as cs meta) =
            Decl.Union
                (recordOriginalNamespace ns name)
                as
                (map constr cs)
                (recordOriginalNamespace ns meta)
        
        constr :: Decl.Constructor -> Decl.Constructor
        constr (Decl.Constructor name args meta) =
            Decl.Constructor
                (recordOriginalNamespace ns name)
                args
                (recordOriginalNamespace ns meta)




recordOriginalNamespace :: (Data.Data a, Data.Typeable a) => ID.Namespace -> a -> a
recordOriginalNamespace ns = Uni.transformBi (f ns)
    where
        f :: ID.Namespace -> Meta.Meta -> Meta.Meta
        f ns meta@Meta.Meta{} =
            Meta.Meta
                { Meta.span = Meta.span meta
                , Meta.inferredType = Meta.inferredType meta
                , Meta.overloadedTargetType = Meta.overloadedTargetType meta
                , Meta.originalNamespace = Just ns
                }

        f ns meta@Meta.Empty{} =
            Meta.Meta
                { Meta.span = Nothing
                , Meta.inferredType = Nothing
                , Meta.overloadedTargetType = Nothing
                , Meta.originalNamespace = Just ns
                }

