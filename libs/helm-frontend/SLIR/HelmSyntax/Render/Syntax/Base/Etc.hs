{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module SLIR.HelmSyntax.Render.Syntax.Base.Etc (
      renderSig
    , renderSig'
) where


-- *
import Core
import Core.Control.Flow ((|>), (<|))

import qualified Data.Maybe as Maybe
import qualified Data.List as List

--- Framework(s)
import Framework.Render
import qualified Framework.Render.Utils as Util

--- Local
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

-- ~ Renderers
import qualified SLIR.HelmSyntax.Render.Syntax.Base.Ident as ID
-- *




-- *
-- | Etc.
-- *


renderSig :: (T.Scheme -> Doc) -> Maybe Etc.Signature -> Doc
renderSig f Nothing = Util.empty
renderSig f (Just sig) =
    renderSig' f sig


renderSig' :: (T.Scheme -> Doc) -> Etc.Signature -> Doc

renderSig' f (Etc.Validated scheme meta) =
    f scheme

renderSig' f (Etc.Unresolved ty meta) =
    f (T.Forall [] ty)


-- renderTypeSig :: (T.Type -> Doc) -> T.Type -> Doc
-- renderTypeSig f (T.Arr t1 t2 meta) =
--     f t1 <+> "->" <+> f t2
-- 
-- renderTypeSig f ty = f ty


-- renderSchemeSig :: (T.Type -> Doc) -> T.Scheme -> Doc
-- renderSchemeSig f (T.Forall [] ty) = renderTypeSig f ty
-- renderSchemeSig f (T.Forall vars ty) =
--     let ty'   = renderTypeSig f ty
--         vars' = map render vars
--           |> Util.punctuate Util.space
--           |> Util.hcat
--     in
--         "forall" <+> vars' <> "." <+> ty'



