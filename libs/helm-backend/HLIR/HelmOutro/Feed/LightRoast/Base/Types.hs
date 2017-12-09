{-# LANGUAGE NoImplicitPrelude #-}
module HLIR.HelmOutro.Feed.LightRoast.Base.Types (
      dropType
    , dropScheme
) where


-- *
import Core

import qualified Data.List as List


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
import qualified HLIR.HelmOutro.Feed.LightRoast.Base.Ident as ID
import qualified HLIR.HelmOutro.Feed.LightRoast.Base.Etc   as Etc
-- *




dropType :: O.T.Type -> L.T.Type

dropType (O.T.Record fields)  =
    L.T.Record $ map field fields
    where
        field (n, t) =
            (ID.dropLow n, dropType t)

dropType (O.T.Tuple ts)  =
    L.T.Tuple $ map dropType ts

dropType (O.T.List ty) =
    L.T.List $ dropType ty

dropType (O.T.Union name args)  =
    L.T.Union
        (ID.dropBig name)
        (map dropType args)

dropType (O.T.Var ident)  =
    L.T.Generic $ Etc.toGeneric ident

dropType t@O.T.Arr{}  =
    let (inTs, outTy) = splitOutput $ flatten t
    in
        L.T.Fn
            (map dropType inTs)
            (Etc.toOutput dropType outTy)



dropType O.T.String  =
    L.T.String

dropType O.T.Char  =
    L.T.Char

dropType O.T.Int  =
    L.T.Int

dropType O.T.Float  =
    L.T.Float

dropType O.T.Bool  =
    L.T.Bool


dropScheme :: O.T.Scheme -> ([L.Etc.Generic], L.Etc.Output)
dropScheme (O.T.Forall as ty) =
    let outTy =
            Etc.toOutput dropType
                $ snd
                $ splitOutput
                $ flatten ty
        
        gs = map Etc.toGeneric as
    in
    
        (gs, outTy)



-- *
-- | Internal Helpers
-- *

-- | Split input ts from output ty.
--
splitOutput :: [O.T.Type] -> ([O.T.Type], O.T.Type)
splitOutput [t] = ([], t)
splitOutput ts
    | List.length ts >= 2 =
        let inTs  = List.init ts
            outTy = List.last ts
        in
            (inTs, outTy)

-- TODO: Error Out with Message...



flatten :: O.T.Type -> [O.T.Type]
flatten (O.T.Arr t1 t2) =
    flatten t1 ++ flatten t2


flatten x = [x]



