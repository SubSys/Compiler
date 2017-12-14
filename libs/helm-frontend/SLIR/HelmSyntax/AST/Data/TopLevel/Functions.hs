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










pattern FnDecl :: IR.Low
               -> [IR.Low]
               -> IR.Expr
               -> Maybe IR.Signature
               -> Maybe IR.Meta
               -> IR.Function

pattern FnDecl name args expr optSig optMeta = IR.FnDecl name args expr optSig optMeta

pattern OpDecl :: IR.Sym
               -> [IR.Low]
               -> IR.Expr
               -> Maybe IR.Signature
               -> Maybe IR.Meta
               -> IR.Function

pattern OpDecl symbol args expr optSig optMeta = IR.OpDecl symbol args expr optSig optMeta







-- *
-- | Internal Helpers
-- *

unwrapLowBinder :: IR.Low -> IR.Low
unwrapLowBinder (IR.Low txt ns meta) =
    IR.Low txt ns meta


unwrapSymBinder :: IR.Sym -> IR.Sym
unwrapSymBinder (IR.Sym txt ns meta) =
    IR.Sym txt ns meta



unwrapArgs :: [IR.Low] -> [IR.Low]
unwrapArgs =
    map unwrapLowBinder



mkFnDecl :: IR.Low
         -> [IR.Low]
         -> IR.Expr
         -> Maybe IR.Signature
         -> Maybe IR.Meta
         -> IR.Function

mkFnDecl (toRefIdent -> ident) (toRefArgs -> args) =
    IR.FnDecl ident args






mkOpDecl :: IR.Sym
         -> [IR.Low]
         -> IR.Expr
         -> Maybe IR.Signature
         -> Maybe IR.Meta
         -> IR.Function

mkOpDecl (toRefSymbol -> ident) (toRefArgs -> args) =
    IR.OpDecl ident args


toRefArgs :: [IR.Low] -> [IR.Low]
toRefArgs = map toRefIdent

toRefIdent ::  IR.Low -> IR.Low
toRefIdent (IR.Low txt ns meta) =
    IR.Low txt ns meta


toRefSymbol :: IR.Sym -> IR.Sym 
toRefSymbol (IR.Sym txt ns meta) =
    IR.Sym txt ns meta




