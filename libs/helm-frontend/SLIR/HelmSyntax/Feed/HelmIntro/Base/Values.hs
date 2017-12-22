{-# LANGUAGE NoImplicitPrelude #-}
module SLIR.HelmSyntax.Feed.HelmIntro.Base.Values (
    dropValue
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
import qualified SLIR.HelmSyntax.Feed.HelmIntro.Base.Types as T
-- *


dropValue :: S.V.LiteralValue -> I.V.LiteralValue
dropValue (S.V.Char val _) =
    I.V.Char val

dropValue (S.V.String val _) =
    I.V.String val

dropValue (S.V.Int val _) =
    I.V.Int val

dropValue (S.V.Float val _) =
    I.V.Float val

dropValue (S.V.Bool val _) =
    I.V.Bool val



