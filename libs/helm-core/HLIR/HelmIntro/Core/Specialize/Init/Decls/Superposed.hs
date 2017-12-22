{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
module HLIR.HelmIntro.Core.Program.Specialize.Init.Decls.Superposed (
    filterSuperposed
) where


-- *
import Core
import Core.Control.Flow ((|>), (<|))
import Core.List.Util    (flatten)

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
import qualified Data.Maybe    as Maybe

import qualified Data.Generics.Uniplate.Data as Uni
import qualified Text.Show.Prettyprint as PP


-- ~ HelmSyntax Misc.
import qualified HLIR.HelmIntro.Render.Utils as Display

-- ~ HelmSyntax IR
import qualified HLIR.HelmIntro.Data.Payload as Payload

--- Local Deps
-- ~ HelmSyntax AST
-- ~~ Base
import qualified HLIR.HelmIntro.AST.Data.Base.Etc      as Etc
import qualified HLIR.HelmIntro.AST.Data.Base.Ident    as ID
import qualified HLIR.HelmIntro.AST.Data.Base.Types    as T
import qualified HLIR.HelmIntro.AST.Data.Base.Values   as V
import qualified HLIR.HelmIntro.AST.Data.Base.Metadata as Meta

-- ~~ TermLevel
import qualified HLIR.HelmIntro.AST.Data.TermLevel.Expressions as E
import qualified HLIR.HelmIntro.AST.Data.TermLevel.Patterns    as P

-- ~~ TopLevel
import qualified HLIR.HelmIntro.AST.Data.TopLevel.Fixities  as Decl
import qualified HLIR.HelmIntro.AST.Data.TopLevel.Functions as Decl
import qualified HLIR.HelmIntro.AST.Data.TopLevel.Unions    as Decl

-- ~~ Header
import qualified HLIR.HelmIntro.AST.Data.Header.Base       as Base
import qualified HLIR.HelmIntro.AST.Data.Header.ImportDecl as Decl


--- Local
-- *



{-# ANN module "HLint: ignore" #-}


filterSuperposed :: [Decl.Function] -> ([Decl.Function], [Decl.Function])
filterSuperposed decls =
    let
        (nonSuper, supers) = M.runWriter (Uni.transformM filterSuperposed' decls)
    in
        (nonSuper, supers)



filterSuperposed' :: M.MonadWriter [Decl.Function] m
                  =>   [Decl.Function]
                  -> m [Decl.Function]
filterSuperposed' xs = do

    ys <- Maybe.catMaybes <$>  M.mapM checkDecl xs

    return xs


checkDecl :: M.MonadWriter [Decl.Function] m => Decl.Function -> m (Maybe Decl.Function)
checkDecl fn@(Decl.FnDecl name args expr (checkSig -> True) meta) = do
    M.tell [fn]
    return Nothing

checkDecl fn = return (Just fn)


checkSig :: Maybe Etc.Signature -> Bool
checkSig sig =
    not (null [ty | (checkType -> (Just ty)) <- Uni.universeBi sig])
    where
        checkType :: T.Type -> Maybe T.Type
        checkType t@(T.Superposed con ts) = Just t
        checkType _ = Nothing


