{-# LANGUAGE NoImplicitPrelude #-}
module CGIR.Metal.Prefabs.PatternMatch.TaggedUnion.Generate where


-- *
import Core
import Core.Control.Flow
import Core.List.Util (flatten)
import Prelude (mapM_, IO, String, return, show)
import Data.List.Index (imap)

import qualified Data.Text as Text
import qualified Data.List as List

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





