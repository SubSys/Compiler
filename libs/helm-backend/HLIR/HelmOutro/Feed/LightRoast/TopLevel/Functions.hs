{-# LANGUAGE NoImplicitPrelude #-}
module HLIR.HelmOutro.Feed.LightRoast.TopLevel.Functions (
    dropFunction
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
import qualified HLIR.HelmOutro.Feed.LightRoast.Base.Ident         as ID
import qualified HLIR.HelmOutro.Feed.LightRoast.Base.Values        as V
import qualified HLIR.HelmOutro.Feed.LightRoast.TermLevel.Patterns as P
import qualified HLIR.HelmOutro.Feed.LightRoast.Base.Etc           as Etc
import qualified HLIR.HelmOutro.Feed.LightRoast.Base.Types         as T
import qualified HLIR.HelmOutro.Feed.LightRoast.TermLevel.Expr     as E
-- *





dropFunction :: O.Decl.Function -> L.Decl.Function
dropFunction (O.Decl.Function name args expr (Just sig)) =
    let (gs, outTy) = T.dropScheme sig
        block       = L.Decl.Block [E.dropExpr expr]
    
    in
        L.Decl.Function
            (ID.dropBinder name)
            gs
            (dropArgs args)
            outTy
            block
    
    
    where
        dropArgs = map (Etc.dropArg T.dropType)
    


-- TODO: Error Out With Message
-- * All Functions should be annotated with it's type scheme, VIA HelmOutro type checker.




