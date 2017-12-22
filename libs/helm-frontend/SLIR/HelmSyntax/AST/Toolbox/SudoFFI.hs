{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module SLIR.HelmSyntax.AST.Toolbox.SudoFFI (
    isSudoNS
  , isSudoNS'
  , callsSudo
  , ifSudoDeclGetTy
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

import qualified Data.Generics.Uniplate.Data as Uni



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
import qualified SLIR.HelmSyntax.AST.Auxiliary.Canonical.Ident as CID
-- *




isSudoNS :: Maybe ID.Namespace -> Bool
isSudoNS (Just (isSudoNS' -> True)) = True
isSudoNS _ = False

isSudoNS' :: ID.Namespace -> Bool
isSudoNS' (ID.Namespace ["Sudo", "Helm", "Native"]) = True
isSudoNS' _ = False



callsSudo :: E.Expr -> Bool
callsSudo expr =
    not $ null [v | (sudoExprVar -> (Just v)) <- Uni.universe expr]
    
    where
        sudoExprVar :: E.Expr -> Maybe E.Expr
        sudoExprVar var@(E.Var (ID.Low _ (isSudoNS -> True) _) _) = Just var
        sudoExprVar _ = Nothing



ifSudoDeclGetTy :: Decl.Function -> Maybe (Either T.Type T.Scheme)
ifSudoDeclGetTy (Decl.FnDecl _ _ (callsSudo -> True) (Just (Etc.Validated scheme _)) _) =
    Just $ Right scheme
ifSudoDeclGetTy (Decl.FnDecl _ _ (callsSudo -> True) (Just (Etc.Unresolved ty _)) _) =
    Just $ Left ty


ifSudoDeclGetTy (Decl.OpDecl _ _ (callsSudo -> True) (Just (Etc.Validated scheme _)) _) =
    Just $ Right scheme
ifSudoDeclGetTy (Decl.OpDecl _ _ (callsSudo -> True) (Just (Etc.Unresolved ty _)) _) =
    Just $ Left ty


ifSudoDeclGetTy _ = Nothing

