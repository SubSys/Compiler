{-# LANGUAGE NoImplicitPrelude #-}
module HLIR.HelmOutro.Feed.LightRoast.TopLevel.Unions (
    dropUnion
) where


-- *
import Core

--- Local Deps
-- ~ HelmOutro AST
-- ~~ Base
import qualified HLIR.HelmOutro.AST.Base.Ident  as O.ID
import qualified HLIR.HelmOutro.AST.Base.Types  as O.T
import qualified HLIR.HelmOutro.AST.Base.Values as O.V
import qualified HLIR.HelmOutro.AST.Base.Etc    as O.Etc
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
import qualified HLIR.HelmOutro.Feed.LightRoast.Base.Ident  as ID
import qualified HLIR.HelmOutro.Feed.LightRoast.Base.Types  as T
-- *



dropUnion :: O.Decl.Union -> L.Decl.Union
dropUnion (O.Decl.Union name tyVars cs) =
    L.Decl.Union
        (ID.dropBig name)
        (map ID.dropLow tyVars)
        (map dropConstructor cs)




dropConstructor :: O.Decl.Constructor -> L.Decl.Constructor
dropConstructor (O.Decl.Constructor name args) =
    L.Decl.Constructor
        (ID.dropBig name)
        (map T.dropType args)








