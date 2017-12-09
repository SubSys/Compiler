{-# LANGUAGE NoImplicitPrelude #-}
module HLIR.HelmCore.Feed.HelmOutro (
    toHelmOutro
) where


-- *
import Core

--- Local Deps
import qualified HLIR.HelmCore.Data.Payload    as CIR
import qualified HLIR.HelmOutro.Data.Payload   as OIR

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
import qualified HLIR.HelmCore.Feed.HelmOutro.TopLevel.Functions as Decl
import qualified HLIR.HelmCore.Feed.HelmOutro.TopLevel.Unions    as Decl
-- *


toHelmOutro :: CIR.Module -> OIR.Module
toHelmOutro payload =
    let functions = CIR.getFunctions payload
        unions    = CIR.getUnions payload
    in
        OIR.Module
            (map Decl.dropFunction functions)
            (map Decl.dropUnion unions)




