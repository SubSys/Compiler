{-# LANGUAGE NoImplicitPrelude #-}
module HLIR.HelmIntro.Feed.HelmCore.Base.Values (
    dropValue
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
import qualified HLIR.HelmIntro.Feed.HelmCore.Base.Ident as ID
import qualified HLIR.HelmIntro.Feed.HelmCore.Base.Types as T
-- *


dropValue :: S.V.LiteralValue -> C.V.LiteralValue
dropValue (S.V.Char val _) =
    C.V.Char val

dropValue (S.V.String val _) =
    C.V.String val

dropValue (S.V.Int val _) =
    C.V.Int val

dropValue (S.V.Float val _) =
    C.V.Float val

dropValue (S.V.Bool val _) =
    C.V.Bool val



