{-# LANGUAGE NoImplicitPrelude #-}
module HLIR.HelmIntro.Feed.HelmCore.TopLevel.Unions (
    dropUnion
) where


-- *
import Core

--- Local Deps
-- ~ HelmSyntax AST
-- ~~ Base
import qualified HLIR.HelmIntro.AST.Data.Base.Etc    as S.Etc
import qualified HLIR.HelmIntro.AST.Data.Base.Ident  as S.ID
import qualified HLIR.HelmIntro.AST.Data.Base.Types  as S.T
import qualified HLIR.HelmIntro.AST.Data.Base.Values as S.V

-- ~~ TermLevel
import qualified HLIR.HelmIntro.AST.Data.TermLevel.Expressions as S.E
import qualified HLIR.HelmIntro.AST.Data.TermLevel.Patterns    as S.P

-- ~~ TopLevel
import qualified HLIR.HelmIntro.AST.Data.TopLevel.Fixities  as S.Decl
import qualified HLIR.HelmIntro.AST.Data.TopLevel.Functions as S.Decl
import qualified HLIR.HelmIntro.AST.Data.TopLevel.Unions    as S.Decl

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
import qualified HLIR.HelmIntro.Feed.HelmCore.Base.Ident  as ID
import qualified HLIR.HelmIntro.Feed.HelmCore.Base.Types  as T
import qualified HLIR.HelmIntro.Feed.HelmCore.Base.Values as V
-- *





dropUnion :: S.Decl.Union -> C.Decl.Union
dropUnion (S.Decl.Union name tyVars cs _) =
    C.Decl.Union
        (ID.dropBig name)
        (map ID.dropLow tyVars)
        (map dropConstructor cs)




dropConstructor :: S.Decl.Constructor -> C.Decl.Constructor
dropConstructor (S.Decl.Constructor name args _) =
    C.Decl.Constructor
        (ID.dropBig name)
        (map T.dropType args)


