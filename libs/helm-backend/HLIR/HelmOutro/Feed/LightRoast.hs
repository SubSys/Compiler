{-# LANGUAGE NoImplicitPrelude #-}
module HLIR.HelmOutro.Feed.LightRoast (
    toLightRoast
) where


-- *
import Core

--- Local Deps
import qualified HLIR.HelmOutro.Data.Payload as HelmOutro
import qualified LLIR.LightRoast.Data.Payload as LightRoast

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
import qualified HLIR.HelmOutro.Feed.LightRoast.Base.Ident         as ID
import qualified HLIR.HelmOutro.Feed.LightRoast.Base.Values        as V
import qualified HLIR.HelmOutro.Feed.LightRoast.TermLevel.Patterns as P
import qualified HLIR.HelmOutro.Feed.LightRoast.Base.Etc           as Etc
import qualified HLIR.HelmOutro.Feed.LightRoast.Base.Types         as T
import qualified HLIR.HelmOutro.Feed.LightRoast.TermLevel.Expr     as E
import qualified HLIR.HelmOutro.Feed.LightRoast.TopLevel.Functions as Decl
import qualified HLIR.HelmOutro.Feed.LightRoast.TopLevel.Unions    as Decl
-- *



toLightRoast :: HelmOutro.Module -> LightRoast.Module
toLightRoast payload =
    let functions = HelmOutro.getFunctions payload
        unions    = HelmOutro.getUnions payload
    in
        LightRoast.Module
            (map Decl.dropFunction functions)
            (map Decl.dropUnion unions)







