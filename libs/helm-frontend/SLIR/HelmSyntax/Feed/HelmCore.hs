{-# LANGUAGE NoImplicitPrelude #-}
module SLIR.HelmSyntax.Feed.HelmCore (
    toHelmCore
) where


-- *
import Core

--- Local Deps

import qualified SLIR.HelmSyntax.Data.Payload as SIR
import qualified HLIR.HelmCore.Data.Payload   as CIR

-- ~ HelmSyntax AST
-- ~~ Base
import qualified SLIR.HelmSyntax.AST.Data.Base.Etc    as S.Etc
import qualified SLIR.HelmSyntax.AST.Data.Base.Ident  as S.ID
import qualified SLIR.HelmSyntax.AST.Data.Base.Types  as S.T
import qualified SLIR.HelmSyntax.AST.Data.Base.Values as S.V

-- ~~ TermLevel
import qualified SLIR.HelmSyntax.AST.Data.TermLevel.Expressions as S.E
import qualified SLIR.HelmSyntax.AST.Data.TermLevel.Patterns    as S.P

-- ~~ TopLevel
import qualified SLIR.HelmSyntax.AST.Data.TopLevel.Fixities  as S.Decl
import qualified SLIR.HelmSyntax.AST.Data.TopLevel.Functions as S.Decl
import qualified SLIR.HelmSyntax.AST.Data.TopLevel.Unions    as S.Decl

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

--- Local
import qualified SLIR.HelmSyntax.Feed.HelmCore.TopLevel.Functions as Decl
import qualified SLIR.HelmSyntax.Feed.HelmCore.TopLevel.Unions    as Decl
-- *


toHelmCore :: SIR.Module -> CIR.Module
toHelmCore payload =
    let functions = SIR.getFunctions payload
        unions    = SIR.getUnions payload
    in
        CIR.Module
            (map Decl.dropFunction functions)
            (map Decl.dropUnion unions)




