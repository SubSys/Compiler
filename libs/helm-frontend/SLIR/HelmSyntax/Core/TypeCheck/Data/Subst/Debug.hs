{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module SLIR.HelmSyntax.Core.TypeCheck.Data.Subst.Debug (
    debugSubst
) where


-- *
import Core
import Core.Control.Flow ((|>), (<|))
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

--- Framework(s)
import Framework.Render
import qualified Framework.Render.Utils as Util
import qualified Framework.Render.Display as Display

--- Local Deps
-- ~ HelmSyntax IR
import qualified SLIR.HelmSyntax.Data.Payload as Payload

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
import qualified SLIR.HelmSyntax.AST.Auxiliary.Canonical.Ident as CID
import qualified SLIR.HelmSyntax.Render.Syntax.Base.Types              as T

import qualified SLIR.HelmSyntax.Core.TypeCheck.Data.Subst                  as Sub
-- *



renderSubst :: Sub.Subst -> Doc
renderSubst (Sub.Subst subs) =
    let subs1 = Map.toAscList subs
        subs2 = map renderSub subs1
            |> Util.vcat
    in
        subs2 <> Util.linebreak


-- renderSubst (Sub.Subst xs subs) =
--     let subs1 = Map.toAscList subs
--         subs2 = map renderSub subs1
--             |> Util.vcat
-- 
--         xs1 = map (renderSubst . Sub.Subst []) xs
--             |> Util.punctuate ","
--             |> Util.vcat
-- 
--     in
--         Util.parens xs1 <> ":" <+> subs2 <> Util.linebreak



renderSub :: (CID.Ident, T.Type) -> Doc
renderSub (CID.Ident txt meta, ty) =
    render txt <+> "=" <+> T.renderType ty



debugSubst :: Sub.Subst -> Text
debugSubst s =
    Display.packDoc' (renderSubst s)




