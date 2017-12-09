{-# LANGUAGE NoImplicitPrelude #-}
module HLIR.HelmCore.Feed.HelmOutro.Base.Ident (
      dropLow
    , dropBig
    , dropRef
    , dropBinder
    , dropNamespace
) where


-- *
import Core

--- Local Deps
-- ~ HelmCore AST
-- ~~ Base
import qualified HLIR.HelmCore.AST.Base.Ident  as C.ID
import qualified HLIR.HelmCore.AST.Base.Types  as C.T
import qualified HLIR.HelmCore.AST.Base.Values as C.V
-- ~~ TermLevel
import qualified HLIR.HelmCore.AST.TermLevel.Expressions as C.E
import qualified HLIR.HelmCore.AST.TermLevel.Patterns    as C.P
-- ~~ TopLevel
import qualified HLIR.HelmCore.AST.TopLevel.Functions as C.Decl
import qualified HLIR.HelmCore.AST.TopLevel.Unions    as C.Decl

-- ~ HelmOutro AST
-- ~ HelmCore AST
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
--- Local
-- *


dropLow :: C.ID.Low -> O.ID.Low
dropLow (C.ID.Low txt ns) =
    O.ID.Low txt (dropNamespace ns)

dropBig :: C.ID.Big -> O.ID.Big
dropBig (C.ID.Big txt ns) =
    O.ID.Big txt (dropNamespace ns)

dropRef :: C.ID.Ref -> O.ID.Ref
dropRef (C.ID.Ref txt ns) =
    O.ID.Ref txt (dropNamespace ns)

dropBinder :: C.ID.Binder -> O.ID.Binder
dropBinder (C.ID.Binder txt ns) =
    O.ID.Binder txt (dropNamespace ns)


dropNamespace :: Maybe C.ID.Namespace -> Maybe O.ID.Namespace
dropNamespace Nothing = Nothing
dropNamespace (Just (C.ID.Namespace segs)) =
    Just $ O.ID.Namespace segs



