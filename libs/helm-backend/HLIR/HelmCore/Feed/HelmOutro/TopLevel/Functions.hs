{-# LANGUAGE NoImplicitPrelude #-}
module HLIR.HelmCore.Feed.HelmOutro.TopLevel.Functions (
    dropFunction
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
import qualified HLIR.HelmCore.Feed.HelmOutro.Base.Ident         as ID
import qualified HLIR.HelmCore.Feed.HelmOutro.Base.Values        as V
import qualified HLIR.HelmCore.Feed.HelmOutro.Base.Types         as T
import qualified HLIR.HelmCore.Feed.HelmOutro.TermLevel.Expr     as E
import qualified HLIR.HelmCore.Feed.HelmOutro.Base.Etc           as Etc
-- *


dropFunction :: C.Decl.Function -> O.Decl.Function
dropFunction (C.Decl.Function name expr scheme) =
    O.Decl.Function
        (ID.dropBinder name)
        []
        (E.dropExpr dropFunction expr)
        (Just $ T.dropScheme scheme)



