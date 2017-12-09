{-# LANGUAGE NoImplicitPrelude #-}
module HLIR.HelmCore.Feed.HelmOutro.Base.Types (
      dropType
    , dropScheme
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
import qualified HLIR.HelmCore.Feed.HelmOutro.Base.Ident as ID
-- *




dropType :: C.T.Type -> O.T.Type

dropType (C.T.Record fields)  =
    O.T.Record $ map field fields
    where
        field (n, t) =
            (ID.dropLow n, dropType t)

dropType (C.T.Tuple ts)  =
    O.T.Tuple $ map dropType ts

dropType (C.T.List ty) =
    O.T.List $ dropType ty

dropType (C.T.Union name args)  =
    O.T.Union
        (ID.dropBig name)
        (map dropType args)

dropType (C.T.Var id')  =
    O.T.Var $ ID.dropLow id'

dropType (C.T.Arr t1 t2)  =
    O.T.Arr
        (dropType t1)
        (dropType t2)


dropType (C.T.String)  =
    O.T.String

dropType (C.T.Char)  =
    O.T.Char

dropType (C.T.Int)  =
    O.T.Int

dropType (C.T.Float)  =
    O.T.Float

dropType (C.T.Bool)  =
    O.T.Bool


dropScheme :: C.T.Scheme -> O.T.Scheme
dropScheme (C.T.Forall as ty) =
    O.T.Forall
        (map ID.dropLow as)
        (dropType ty)






