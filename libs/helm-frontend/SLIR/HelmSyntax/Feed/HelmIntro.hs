{-# LANGUAGE NoImplicitPrelude #-}
module SLIR.HelmSyntax.Feed.HelmIntro (
    toHelmIntro
) where


-- *
import Core

--- Local Deps

import qualified SLIR.HelmSyntax.Data.Interface.Program.Payload as SIR
import qualified HLIR.HelmIntro.Data.Payload   as CIR

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

-- ~ HelmIntro AST
-- ~~ Base
import qualified HLIR.HelmIntro.AST.Data.Base.Ident  as I.ID
import qualified HLIR.HelmIntro.AST.Data.Base.Types  as I.T
import qualified HLIR.HelmIntro.AST.Data.Base.Values as I.V
-- ~~ TermLevel
import qualified HLIR.HelmIntro.AST.Data.TermLevel.Expressions as I.E
import qualified HLIR.HelmIntro.AST.Data.TermLevel.Patterns    as I.P
-- ~~ TopLevel
import qualified HLIR.HelmIntro.AST.Data.TopLevel.Functions as I.Decl
import qualified HLIR.HelmIntro.AST.Data.TopLevel.Unions    as I.Decl


--- Local
import qualified SLIR.HelmSyntax.Feed.HelmIntro.TopLevel.Functions as Decl
import qualified SLIR.HelmSyntax.Feed.HelmIntro.TopLevel.Unions    as Decl
-- *


toHelmIntro :: SIR.Program -> CIR.Program
toHelmIntro payload =
    let functions = SIR.getFunctions payload
        unions    = SIR.getUnions payload
    in
        CIR.Program
            (map Decl.dropUnion unions)
            (map Decl.dropFunction functions)




