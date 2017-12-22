{-# LANGUAGE NoImplicitPrelude #-}
module SLIR.HelmSyntax.Feed.HelmIntro.Base.Ident (
      dropLow
    , dropBig
    , dropNamespace
    , toRef
    , toBinder
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
-- *


dropLow :: S.ID.Low -> I.ID.Low
dropLow (S.ID.Low txt ns _) =
    I.ID.Low txt (dropNamespace ns)

dropBig :: S.ID.Big -> I.ID.Big
dropBig (S.ID.Big txt ns _) =
    I.ID.Big txt (dropNamespace ns)


dropNamespace :: Maybe S.ID.Namespace -> Maybe I.ID.Namespace
dropNamespace Nothing = Nothing
dropNamespace (Just (S.ID.Namespace segs)) =
    Just $ I.ID.Namespace segs


toRef :: S.ID.Low -> I.ID.Ref
toRef (S.ID.Low txt ns _) =
    I.ID.Ref txt (dropNamespace ns)

toBinder :: S.ID.Low -> I.ID.Binder
toBinder (S.ID.Low txt ns _) =
    I.ID.Binder txt (dropNamespace ns)


