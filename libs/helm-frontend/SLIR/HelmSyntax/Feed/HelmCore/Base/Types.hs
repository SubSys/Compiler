{-# LANGUAGE NoImplicitPrelude #-}
module SLIR.HelmSyntax.Feed.HelmCore.Base.Types (
      dropType
    , dropScheme
) where


-- *
import Core

--- Local Deps
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
import qualified SLIR.HelmSyntax.Feed.HelmCore.Base.Ident as ID
-- *

dropType :: S.T.Type -> C.T.Type

dropType (S.T.Record fields _)  =
    C.T.Record $ map field fields
    where
        field (n, t) =
            (ID.dropLow n, dropType t)

dropType (S.T.Tuple ts _)  =
    C.T.Tuple $ map dropType ts

dropType (S.T.List ty _) =
    C.T.List $ dropType ty

dropType (S.T.Union name args _)  =
    C.T.Union
        (ID.dropBig name)
        (map dropType args)

dropType (S.T.Var id' _)  =
    C.T.Var $ ID.dropLow id'

dropType (S.T.Arr t1 t2 _)  =
    C.T.Arr
        (dropType t1)
        (dropType t2)

dropType (S.T.Parens ty _)  =
    dropType ty

dropType (S.T.String _)  =
    C.T.String

dropType (S.T.Char _)  =
    C.T.Char

dropType (S.T.Int _)  =
    C.T.Int

dropType (S.T.Float _)  =
    C.T.Float

dropType (S.T.Bool _)  =
    C.T.Bool


dropScheme :: S.T.Scheme -> C.T.Scheme
dropScheme (S.T.Forall as ty) =
    C.T.Forall
        (map ID.dropLow as)
        (dropType ty)


