{-# LANGUAGE NoImplicitPrelude #-}
module SLIR.HelmSyntax.Feed.HelmIntro.TopLevel.Functions where


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
import qualified SLIR.HelmSyntax.Feed.HelmIntro.Base.Ident  as ID
import qualified SLIR.HelmSyntax.Feed.HelmIntro.Base.Types  as T
import qualified SLIR.HelmSyntax.Feed.HelmIntro.Base.Values as V
import qualified SLIR.HelmSyntax.Feed.HelmIntro.TermLevel.Expressions as E
-- *




dropFunction :: S.Decl.Function -> I.Decl.Function
dropFunction (S.Decl.FnDecl name args expr (Just (S.Etc.Validated scheme _)) _) =
    I.Decl.Function
        (ID.toBinder name)
        (map ID.toBinder args)
        (E.dropExpr dropFunction expr)
        (Just $ T.dropScheme scheme)


dropFunction (S.Decl.OpDecl (S.ID.Sym txt ns _) args expr (Just (S.Etc.Validated scheme _)) _) =
    I.Decl.Function
        (I.ID.Binder txt (ID.dropNamespace ns))
        (map ID.toBinder args)
        (E.dropExpr dropFunction expr)
        (Just $ T.dropScheme scheme)






