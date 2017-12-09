{-# LANGUAGE NoImplicitPrelude #-}
module CGIR.Metal.Dev.Labs.TypeSystem.TaggedUnion where


-- *
import Core
import Core.Control.Flow
import Core.List.Util (flatten)

import Prelude (mapM_, IO, String, return)

import qualified Data.Text as Text


--- Dev
import qualified Dev.Samples.Basic      as BasicSamples
import qualified Dev.Samples.Complex    as ComplexSamples
import qualified Dev.Samples.TestParser as ParserSample

import qualified Text.Show.Prettyprint as PP

--- Local
import qualified CGIR.Metal.Render.Utils as Utils

-- ~ Metal AST
-- ~~ Base
import qualified CGIR.Metal.AST.Base.Ident       as ID
import qualified CGIR.Metal.AST.Base.Types       as T
import qualified CGIR.Metal.AST.Base.Values      as V
-- ~~ BlockLevel
import qualified CGIR.Metal.AST.BlockLevel.Stmts   as S
import qualified CGIR.Metal.AST.BlockLevel.Members as Member
-- ~~ TopLevel
import qualified CGIR.Metal.AST.TopLevel.Decls   as Decl
import qualified CGIR.Metal.AST.TopLevel.Decls.Header.Parameters as Param
-- *







