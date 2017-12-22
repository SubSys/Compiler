{-# LANGUAGE NoImplicitPrelude #-}
module HLIR.HelmIntro.Feed.HelmCore.Base.Ident (
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
-- *


dropLow :: S.ID.Low -> C.ID.Low
dropLow (S.ID.Low txt ns _) =
    C.ID.Low txt (dropNamespace ns)

dropBig :: S.ID.Big -> C.ID.Big
dropBig (S.ID.Big txt ns _) =
    C.ID.Big txt (dropNamespace ns)


dropNamespace :: Maybe S.ID.Namespace -> Maybe C.ID.Namespace
dropNamespace Nothing = Nothing
dropNamespace (Just (S.ID.Namespace segs)) =
    Just $ C.ID.Namespace segs


toRef :: S.ID.Low -> C.ID.Ref
toRef (S.ID.Low txt ns _) =
    C.ID.Ref txt (dropNamespace ns)

toBinder :: S.ID.Low -> C.ID.Binder
toBinder (S.ID.Low txt ns _) =
    C.ID.Binder txt (dropNamespace ns)

