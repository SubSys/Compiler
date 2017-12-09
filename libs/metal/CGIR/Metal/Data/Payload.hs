{-# LANGUAGE NoImplicitPrelude #-}
module CGIR.Metal.Data.Payload (
      Payload(..)
    , Program(..)
) where


-- *
import Core

--- Framework
import qualified Framework.Pipeline.Data as Pipe

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


type Payload = Pipe.Payload Program


data Program = Program
    { program :: [Decl.FunctionDecl]
    , structs :: [Decl.StructDecl]
    }
    deriving (Show)