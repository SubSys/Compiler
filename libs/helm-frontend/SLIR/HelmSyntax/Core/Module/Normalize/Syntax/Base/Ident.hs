{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns #-}
module SLIR.HelmSyntax.Core.Module.Normalize.Syntax.Base.Ident (
    normLow
  , normBig
  , normSym
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

-- ~ Aux. Nodes & Utils
import qualified SLIR.HelmSyntax.AST.Auxiliary.Canonical.Ident as CID
import qualified SLIR.HelmSyntax.AST.Toolbox.Unions.Utils      as Union

--- Local
import qualified SLIR.HelmSyntax.Core.Module.Normalize.Utils.Namespace as NS
import qualified SLIR.HelmSyntax.Core.Module.Normalize.Data.System     as Sys
-- *




normLow :: ID.Low -> Sys.Env ID.Low
normLow ident@(ID.Low _ (isSudoNS -> True) _) =
    return ident
    
normLow ident@(ID.Low name ns meta) = do
    decls <- Payload.getFunctions <$> M.ask
    modulename <- Payload.getModuleName <$> M.ask
    deps <- Payload.getDependencies <$> M.ask
    
    if isLocal ident (Right decls) then
        return
            $ ID.Low name (Just modulename) meta
    else
        case lookupFunDeps ident deps  of
            Just segs ->
                return
                    $ ID.Low name (Just $ ID.Namespace segs) meta
            
            Nothing ->
                return ident


normSym :: ID.Sym -> Sys.Env ID.Sym
normSym ident@(ID.Sym _ (isSudoNS -> True) _) =
    return ident
    
normSym ident@(ID.Sym name ns meta) = do
    decls <- Payload.getFunctions <$> M.ask
    modulename <- Payload.getModuleName <$> M.ask
    deps <- Payload.getDependencies <$> M.ask
    
    if isLocal ident (Right decls) then
        return
            $ ID.Sym name (Just modulename) meta
    else
        case lookupFunDeps ident deps  of
            Just segs ->
                return
                    $ ID.Sym name (Just $ ID.Namespace segs) meta
            
            Nothing ->
                return ident



normBig :: ID.Big -> Sys.Env ID.Big
normBig ident@(ID.Big _ (isSudoNS -> True) _) =
    return ident
    
normBig ident@(ID.Big name ns meta) = do
    decls <- Payload.getUnions <$> M.ask
    modulename <- Payload.getModuleName <$> M.ask
    
    if isLocal ident (Left decls) then
        return
            $ ID.Big name (Just modulename) meta
    else
        case lookupOrigNS meta of
            Just segs ->
                return
                    $ ID.Big name (Just $ ID.Namespace segs) meta
            
            Nothing ->
                return ident





-- *
-- | Internal Helpers
-- *

lookupOrigNS :: Maybe Meta.Meta -> Maybe [Text]
lookupOrigNS Nothing = Nothing
lookupOrigNS (Just meta) =
    Meta.originalNamespace meta



-- | I.e.
-- * `import This.That as That` 
-- ** then update `x = That.y`, into `x = This.That.y`

-- normNSAlias :: ID.Namespace -> Sys.Env ID.Namespace
-- normNSAlias ns@(ID.Namespace segs) = do
--     payload <- M.ask

-- lookupLocal :: ID.Low -> Sys.Env (Maybe ID.Namespace)
-- lookupLocal name = do
-- 
--     payload <- M.ask
-- 
--     let moduleName = Payload.getModuleName payload
--         decls      = Payload.getFunctions payload



isLocal :: CID.Identifiable a => a -> Either [Decl.Union] [Decl.Function] -> Bool
isLocal name (Right decls) =
    case List.find (== CID.ident name) (CID.idents decls) of
        Just{} -> True
        Nothing -> False

isLocal name (Left decls) =
    case List.find (== CID.ident name) (CID.idents decls) of
        Just{} -> True
        Nothing ->
            case Union.lookupUnion (CID.toBig $ CID.ident name) decls of
                Nothing -> False
                Just{}  -> True


isSudoNS :: Maybe ID.Namespace -> Bool
isSudoNS Nothing = False
isSudoNS (Just (ID.Namespace segs)) =
    let
        sudo = Text.pack "Sudo"
        helm = Text.pack "Helm"
        native = Text.pack "Native"
    in
        segs == [sudo, helm, native]




lookupFunDeps :: CID.Identifiable a => a -> Payload.Dependencies -> Maybe [Text]
lookupFunDeps name deps =
    let
        decls = Payload.functionDeps deps
    in
        case List.find pred decls of
            Nothing -> Nothing
            Just fn ->
                extractOrigFnNS fn
    
    where
        pred :: Decl.Function -> Bool
        pred (Decl.FnDecl x _ _ _ _) =
            x == CID.toLow (CID.ident name)
        
        pred (Decl.OpDecl x _ _ _ _) =
            x == CID.toSym (CID.ident name)


extractOrigFnNS :: Decl.Function -> Maybe [Text]
extractOrigFnNS (Decl.FnDecl (ID.Low _ _ (Just meta)) _ _ _ _) =
    Meta.originalNamespace meta
extractOrigFnNS (Decl.OpDecl (ID.Sym _ _ (Just meta)) _ _ _ _) =
    Meta.originalNamespace meta

