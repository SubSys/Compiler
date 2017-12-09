{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module SLIR.HelmSyntax.AST.Data.TopLevel.Functions (
      IR.Function
    , pattern FnDecl
    , pattern OpDecl
) where


-- *
import Core
import Prelude (show)

import qualified Data.Text as Text

import Data.Data (Data, Typeable)
import GHC.Types (Constraint)
import qualified Data.Data as Data

--- Local
import qualified SLIR.HelmSyntax.Internal.AST as IR

-- ~ AST - Essential Instances
import SLIR.HelmSyntax.Internal.AST.Instances.Essential ()
-- *










pattern FnDecl :: IR.Low Text
               -> [IR.Low Text]
               -> IR.Expr
               -> Maybe IR.Signature
               -> Maybe IR.Meta
               -> IR.Function

pattern FnDecl name args expr optSig optMeta
    <- IR.FnDecl (unwrapLowBinder -> name) (unwrapArgs -> args) expr optSig optMeta
    where
        FnDecl name args expr optSig optMeta = mkFnDecl name args expr optSig optMeta


pattern OpDecl :: IR.Sym Text
               -> [IR.Low Text]
               -> IR.Expr
               -> Maybe IR.Signature
               -> Maybe IR.Meta
               -> IR.Function

pattern OpDecl symbol args expr optSig optMeta
    <- IR.OpDecl (unwrapSymBinder -> symbol) (unwrapArgs -> args) expr optSig optMeta
    where
        OpDecl symbol args expr optSig optMeta = mkOpDecl symbol args expr optSig optMeta








-- *
-- | Internal Helpers
-- *

unwrapLowBinder :: IR.Low IR.Binder -> IR.Low Text
unwrapLowBinder (IR.Low (IR.Binder txt) ns meta) =
    IR.Low txt ns meta

unwrapLowBinder (IR.Low (IR.BinderIndex idx) ns meta) =
    let txt = Text.pack $ show idx
    in
        IR.Low (Text.cons 'λ' txt) ns meta


unwrapSymBinder :: IR.Sym IR.Binder -> IR.Sym Text
unwrapSymBinder (IR.Sym (IR.Binder txt) ns meta) =
    IR.Sym txt ns meta

unwrapSymBinder (IR.Sym (IR.BinderIndex idx) ns meta) =
    let txt = Text.pack $ show idx
    in
        IR.Sym (Text.cons 'λ' txt) ns meta


unwrapArgs :: [IR.Low IR.Binder] -> [IR.Low Text]
unwrapArgs =
    map unwrapLowBinder



mkFnDecl :: IR.Low Text
         -> [IR.Low Text]
         -> IR.Expr
         -> Maybe IR.Signature
         -> Maybe IR.Meta
         -> IR.Function

mkFnDecl (toRefIdent -> ident) (toRefArgs -> args) =
    IR.FnDecl ident args






mkOpDecl :: IR.Sym Text
         -> [IR.Low Text]
         -> IR.Expr
         -> Maybe IR.Signature
         -> Maybe IR.Meta
         -> IR.Function

mkOpDecl (toRefSymbol -> ident) (toRefArgs -> args) =
    IR.OpDecl ident args


toRefArgs :: [IR.Low Text] -> [IR.Low IR.Binder]
toRefArgs = map toRefIdent

toRefIdent ::  IR.Low Text -> IR.Low IR.Binder
toRefIdent (IR.Low txt ns meta) =
    IR.Low (IR.Binder txt) ns meta


toRefSymbol :: IR.Sym Text -> IR.Sym IR.Binder
toRefSymbol (IR.Sym txt ns meta) =
    IR.Sym (IR.Binder txt) ns meta




