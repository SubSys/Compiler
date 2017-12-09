{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
module CGIR.Metal.Render.Instances.Helpers (
      renderIndexValue
    , renderDimValue
) where


-- *
import Core
import Core.Control.Flow

import qualified Data.Maybe as Maybe
import qualified Data.List as List

--- Framework(s)
import Framework.Render'
import qualified Framework.Render'.Utils as Util

--- Local



-- ~ Metal AST
-- ~~ Base
import qualified CGIR.Metal.AST.Base.Ident       as ID

import qualified CGIR.Metal.AST.Base.Types       as T
import qualified CGIR.Metal.AST.Base.Types.Aux   as T

import qualified CGIR.Metal.AST.Base.Values      as V
import qualified CGIR.Metal.AST.Base.Values.Aux  as V

-- ~~ BlockLevel
import qualified CGIR.Metal.AST.BlockLevel.Stmts     as S
import qualified CGIR.Metal.AST.BlockLevel.Stmts.Aux as S

import qualified CGIR.Metal.AST.BlockLevel.Members as Member

-- ~~ TopLevel
import qualified CGIR.Metal.AST.TopLevel.Decls   as Decl
import qualified CGIR.Metal.AST.TopLevel.Decls.Header.Parameters as Header
-- *




















-- | Helpers for Value Instances
--

renderIndexValue :: Render a => Doc -> V.IndexValue a -> Doc
renderIndexValue label (V.Index2Value x1 x2) =
    let xs     = map render [x1, x2]
        parens = Util.punctuate "," xs
            |> Util.hcat
            |> Util.parens
    in
        label <> "2" <> parens


renderIndexValue label (V.Index3Value x1 x2 x3) =
    let xs     = map render [x1, x2, x3]
        parens = Util.punctuate "," xs
            |> Util.hcat
            |> Util.parens
    in
        label <> "3" <> parens

renderIndexValue label (V.Index4Value x1 x2 x3 x4) =
    let xs     = map render [x1, x2, x3, x4]
        parens = Util.punctuate "," xs
            |> Util.hcat
            |> Util.parens
    in
        label <> "4" <> parens



renderDimValue :: Render a => Doc -> V.DimValue a -> Doc
renderDimValue label (V.Dim2x2Value (x1, x2) (y1, y2)) =
    let xs     = map render [x1, x2, y1, y2]
        parens = Util.punctuate "," xs
            |> Util.hcat
            |> Util.parens
    in
        label <> "2x2" <> parens

renderDimValue label (V.Dim2x3Value
        (x1, x2)
        (y1, y2)
        (z1, z2)) =

    let xs     = map render [x1, x2, y1, y2, z1, z2]
        parens = Util.punctuate "," xs
            |> Util.hcat
            |> Util.parens
    in
        label <> "2x3" <> parens

renderDimValue label (V.Dim2x4Value
        (a1, a2)
        (b1, b2)
        (c1, c2)
        (d1, d2)) =
    let xs     = map render [a1, a2, b1, b2, c1, c2, d1, d2]
        parens = Util.punctuate "," xs
            |> Util.hcat
            |> Util.parens
    in
        label <> "2x4" <> parens

renderDimValue label (V.Dim3x2Value
        (a1, a2, a3)
        (b1, b2, b3)) =
    let xs     = map render [a1, a2, a3, b1, b2, b3]
        parens = Util.punctuate "," xs
            |> Util.hcat
            |> Util.parens
    in
        label <> "3x2" <> parens

renderDimValue label (V.Dim3x3Value
        (a1, a2, a3)
        (b1, b2, b3)
        (c1, c2, c3)) =
    let xs     = map render [a1, a2, a3, b1, b2, b3, c1, c2, c3]
        parens = Util.punctuate "," xs
            |> Util.hcat
            |> Util.parens
    in
        label <> "3x3" <> parens

renderDimValue label (V.Dim3x4Value
        (a1, a2, a3)
        (b1, b2, b3)
        (c1, c2, c3)
        (d1, d2, d3)) =
    let xs     = map render [a1, a2, a3, b1, b2, b3, c1, c2, c3, d1, d2, d3]
        parens = Util.punctuate "," xs
            |> Util.hcat
            |> Util.parens
    in
        label <> "3x4" <> parens

renderDimValue label (V.Dim4x2Value
        (a1, a2, a3, a4)
        (b1, b2, b3, b4)) =
    let xs     = map render [a1, a2, a3, a4, b1, b2, b3, b4]
        parens = Util.punctuate "," xs
            |> Util.hcat
            |> Util.parens
    in
        label <> "4x2" <> parens

renderDimValue label (V.Dim4x3Value
        (a1, a2, a3, a4)
        (b1, b2, b3, b4)
        (c1, c2, c3, c4)) =
    let xs     = map render [a1, a2, a3, a4, b1, b2, b3, b4, c1, c2, c3, c4]
        parens = Util.punctuate "," xs
            |> Util.hcat
            |> Util.parens
    in
        label <> "4x3" <> parens

renderDimValue label (V.Dim4x4Value
        (a1, a2, a3, a4)
        (b1, b2, b3, b4)
        (c1, c2, c3, c4)
        (d1, d2, d3, d4)) =
    let xs     = map render [a1, a2, a3, a4, b1, b2, b3, b4, c1, c2, c3, c4, d1, d2, d3, d4]
        parens = Util.punctuate "," xs
            |> Util.hcat
            |> Util.parens
    in
        label <> "4x4" <> parens




