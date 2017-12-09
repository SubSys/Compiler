{-# LANGUAGE NoImplicitPrelude #-}
module CGIR.Metal.Render.Utils (
      renderFunctions
    , renderStructs
    , renderUnions
    , renderEnums
) where


-- *
import Core
import Core.Control.Flow

import qualified Data.Maybe as Maybe
import qualified Data.List as List
import qualified Data.Text as Text



--- Framework(s)
-- ~ Renderer
import Framework.Render'
import qualified Framework.Render'.Utils as Util
import qualified Framework.Render'.Display as Display

-- ~ Pipeline & Related
import qualified Framework.Pipeline.Data    as Pipe
import qualified Framework.IR.Standard.Data as StdIR


--- Local
import CGIR.Metal.Render.Instances ()




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





renderFunctions :: [Decl.FunctionDecl] -> Text
renderFunctions xs =
    map Display.packDoc xs
        |> Text.unlines


renderStructs :: [Decl.StructDecl] -> Text
renderStructs xs =
    map Display.packDoc xs
        |> Text.unlines


renderUnions :: [Decl.UnionDecl] -> Text
renderUnions xs =
    map Display.packDoc xs
        |> Text.unlines


renderEnums :: [Decl.EnumClassDecl] -> Text
renderEnums xs =
    map Display.packDoc xs
        |> Text.unlines

