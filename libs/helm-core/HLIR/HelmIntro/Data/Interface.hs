{-# LANGUAGE NoImplicitPrelude #-}
module HLIR.HelmIntro.Data.Interface where


-- *
import Core


--- Local
-- ~ HelmSyntax Interfaces
import qualified HLIR.HelmIntro.Data.Payload as Program
import qualified HLIR.HelmIntro.Data.Interface.Module.Payload   as Payload

-- ~ HelmSyntax AST
-- ~~ Base
import qualified HLIR.HelmIntro.AST.Data.Base.Etc    as Etc
import qualified HLIR.HelmIntro.AST.Data.Base.Ident  as ID
import qualified HLIR.HelmIntro.AST.Data.Base.Types  as T
import qualified HLIR.HelmIntro.AST.Data.Base.Values as V

-- ~~ TermLevel
import qualified HLIR.HelmIntro.AST.Data.TermLevel.Expressions as E
import qualified HLIR.HelmIntro.AST.Data.TermLevel.Patterns    as P

-- ~~ TopLevel
import qualified HLIR.HelmIntro.AST.Data.TopLevel.Fixities  as Decl
import qualified HLIR.HelmIntro.AST.Data.TopLevel.Functions as Decl
import qualified HLIR.HelmIntro.AST.Data.TopLevel.Unions    as Decl

-- ~~ Header
import qualified HLIR.HelmIntro.AST.Data.Header.Base       as Base
import qualified HLIR.HelmIntro.AST.Data.Header.ImportDecl as Decl
-- *





