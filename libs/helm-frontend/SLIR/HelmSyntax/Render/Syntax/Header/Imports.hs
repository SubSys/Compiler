{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module SLIR.HelmSyntax.Render.Syntax.Header.Imports where


-- *
import Core
import Core.Control.Flow ((|>), (<|))

import qualified Data.Maybe as Maybe
import qualified Data.List as List

--- Framework(s)
import Framework.Render
import qualified Framework.Render.Utils as Util

--- Local
-- ~ HelmSyntax IR
import qualified SLIR.HelmSyntax.Data.Payload as Payload

-- ~ HelmSyntax AST
-- ~~ Base
import qualified SLIR.HelmSyntax.AST.Data.Base.Etc    as Etc
import qualified SLIR.HelmSyntax.AST.Data.Base.Ident  as ID
import qualified SLIR.HelmSyntax.AST.Data.Base.Types  as T
import qualified SLIR.HelmSyntax.AST.Data.Base.Values as V

-- ~~ TermLevel
import qualified SLIR.HelmSyntax.AST.Data.TermLevel.Expressions as E
import qualified SLIR.HelmSyntax.AST.Data.TermLevel.Patterns    as P

-- ~~ TopLevel
import qualified SLIR.HelmSyntax.AST.Data.TopLevel.Fixities  as Decl
import qualified SLIR.HelmSyntax.AST.Data.TopLevel.Functions as Decl
import qualified SLIR.HelmSyntax.AST.Data.TopLevel.Unions    as Decl

-- ~ Renderers
import qualified SLIR.HelmSyntax.Render.Syntax.Base.Ident as ID
-- *




-- instance Render Import.ModuleImporting where
--     render (Import.ModuleImporting importDeclarations) =
--         map render importDeclarations
--             |> Util.punctuate Util.linebreak
--             |> Util.vcat





-- *
-- | Import Declarations
-- *
-- instance Render Import.ImportDecl where
--     render (Import.Qualified ns Nothing) =
--         "import" <+> render ns
-- 
--     render (Import.Qualified ns (Just asAlias)) =
--         "import" <+> render ns <+> "as" <+> render asAlias
-- 
-- 
--     render (Import.Explicit ns entries) =
--         "import" <+> render ns <+> renderEntries entries
-- 
--     render (Import.Everything ns) =
--         "import" <+> render ns <+> "(..)"
-- 
-- 
-- 
-- renderEntries :: [Header.Entry] -> Doc
-- renderEntries entries =
--     map render entries
--           |> Util.punctuate ","
--           |> Util.hcat
--           |> Util.parens
-- 


