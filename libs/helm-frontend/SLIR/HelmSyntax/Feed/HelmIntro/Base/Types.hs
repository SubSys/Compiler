{-# LANGUAGE NoImplicitPrelude #-}
module SLIR.HelmSyntax.Feed.HelmIntro.Base.Types (
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
import qualified SLIR.HelmSyntax.Feed.HelmIntro.Base.Ident as ID
-- *

dropType :: S.T.Type -> I.T.Type

dropType (S.T.Record fields _)  =
    I.T.Record $ map field fields
    where
        field (n, t) =
            (ID.dropLow n, dropType t)

dropType (S.T.Tuple ts _)  =
    I.T.Tuple $ map dropType ts

dropType (S.T.List ty _) =
    I.T.List $ dropType ty

dropType (S.T.Union name args _)  =
    I.T.Union
        (ID.dropBig name)
        (map dropType args)

dropType (S.T.Var id' _)  =
    I.T.Var $ ID.dropLow id'

dropType (S.T.Arr t1 t2 _)  =
    I.T.Arr
        (dropType t1)
        (dropType t2)

dropType (S.T.Parens ty _)  =
    dropType ty

dropType (S.T.String _)  =
    I.T.String

dropType (S.T.Char _)  =
    I.T.Char

dropType (S.T.Int _)  =
    I.T.Int

dropType (S.T.Float _)  =
    I.T.Float

dropType (S.T.Bool _)  =
    I.T.Bool


dropType (S.T.Superposed x ts)  =
    I.T.Superposed
        (dropType x)
        (map dropType ts)


dropScheme :: S.T.Scheme -> I.T.Scheme
dropScheme (S.T.Forall as ty) =
    I.T.Forall
        (map ID.dropLow as)
        (dropType ty)


