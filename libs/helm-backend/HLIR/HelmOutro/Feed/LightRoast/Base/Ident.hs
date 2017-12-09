{-# LANGUAGE NoImplicitPrelude #-}
module HLIR.HelmOutro.Feed.LightRoast.Base.Ident (
      dropLow
    , dropBig
    , dropRef
    , dropBinder
    , dropNamespace
) where


-- *
import Core

--- Local Deps
-- ~ HelmOutro AST
-- ~~ Base
import qualified HLIR.HelmOutro.AST.Base.Ident  as O.ID
import qualified HLIR.HelmOutro.AST.Base.Types  as O.T
import qualified HLIR.HelmOutro.AST.Base.Values as O.V
-- ~~ TermLevel
import qualified HLIR.HelmOutro.AST.TermLevel.Expressions as O.E
import qualified HLIR.HelmOutro.AST.TermLevel.Patterns    as O.P
-- ~~ TopLevel
import qualified HLIR.HelmOutro.AST.TopLevel.Functions as O.Decl
import qualified HLIR.HelmOutro.AST.TopLevel.Unions    as O.Decl


-- ~ LightRoast AST
-- ~~ Base
import qualified LLIR.LightRoast.AST.Base.Etc           as L.Etc
import qualified LLIR.LightRoast.AST.Base.Ident         as L.ID
import qualified LLIR.LightRoast.AST.Base.Types         as L.T
import qualified LLIR.LightRoast.AST.Base.Values        as L.V
-- ~~ TermLevel
import qualified LLIR.LightRoast.AST.TermLevel.Block    as L.Decl
import qualified LLIR.LightRoast.AST.TermLevel.Patterns as L.P
import qualified LLIR.LightRoast.AST.TermLevel.Stmt     as L.S
-- ~~ TopLevel
import qualified LLIR.LightRoast.AST.TopLevel.Functions as L.Decl
import qualified LLIR.LightRoast.AST.TopLevel.Unions    as L.Decl

--- Local
-- *





dropLow :: O.ID.Low -> L.ID.Low
dropLow (O.ID.Low txt ns) =
    L.ID.Low txt (dropNamespace ns)


dropBig :: O.ID.Big -> L.ID.Big
dropBig (O.ID.Big txt ns) =
    L.ID.Big txt (dropNamespace ns)


dropRef :: O.ID.Ref -> L.ID.Low
dropRef (O.ID.Ref txt ns) =
    L.ID.Low txt (dropNamespace ns)

dropBinder :: O.ID.Binder -> L.ID.Low
dropBinder (O.ID.Binder txt ns) =
    L.ID.Low txt (dropNamespace ns)



dropNamespace :: Maybe O.ID.Namespace -> Maybe L.ID.Namespace
dropNamespace Nothing = Nothing
dropNamespace (Just (O.ID.Namespace segs)) =
    Just $ L.ID.Namespace segs



