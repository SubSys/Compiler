module SLIR.HelmSyntax.Module.Core.Parser.Syntax.Header.ImportDecl where


-- *
import Core
import Core.Control.Flow ((|>), (<|))
import Core.List.Util    (flatten, singleton)
import Data.Monoid ((<>))
import Prelude
    (return
    , String
    , IO
    , show
    , error
    , (<$>)
    , (>>=)
    , (>>)
    , fromIntegral
    )

import qualified Prelude as Pre


import qualified Control.Monad              as M
import qualified Control.Monad.State        as M
import qualified Control.Monad.Except       as M
import qualified Control.Monad.RWS          as M
import qualified Control.Monad.Identity     as M
import qualified Control.Monad.Reader       as M
import qualified Control.Monad.Writer       as M
import qualified Control.Monad.Trans        as M

import qualified Data.List                    as List
import qualified Data.Text                    as Text
import qualified Data.Text.IO                 as TIO
import qualified Data.Map                     as Map
import qualified Data.Set                     as Set
import qualified Data.Foldable                as Fold
import qualified Data.Monoid                  as Monoid
import qualified Data.Maybe                   as Maybe
import qualified Data.Either                  as Either
import qualified Data.Char                    as Char
import qualified Data.Word                    as Word
import qualified Data.STRef                   as ST
import qualified Data.Bits                    as Bit
import qualified Data.Fixed                   as Fixed
import qualified Data.Vector.Unboxed          as V
import qualified Data.Vector.Unboxed.Mutable  as MV
import qualified Data.Vector.Generic          as VG
import qualified Data.IORef                   as IORef
import qualified Data.ByteString              as BS
import qualified Data.Functor                 as Fun


-- + Recursion Schemes & Related
import qualified Data.Functor.Foldable as F


-- + Megaparsec & Related
import qualified Text.Megaparsec.Char       as C
import qualified Text.Megaparsec.Char.Lexer as L

-- + Frameworks
import Framework.Text.Parser

-- + Dev & Debugging
import qualified Text.Show.Prettyprint as PP

-- + HelmSyntax AST
-- ++ Base
import qualified SLIR.HelmSyntax.AST.Data.Semantic.Base.Etc      as Etc
import qualified SLIR.HelmSyntax.AST.Data.Semantic.Base.Ident    as ID
import qualified SLIR.HelmSyntax.AST.Data.Semantic.Base.Types    as T
import qualified SLIR.HelmSyntax.AST.Data.Semantic.Base.Values   as V
import qualified SLIR.HelmSyntax.AST.Data.Semantic.Base.Metadata as Meta
import qualified SLIR.HelmSyntax.AST.Data.Semantic.Base.Header   as Header

-- ++ TermLevel
import qualified SLIR.HelmSyntax.AST.Data.Semantic.TermLevel.Expr     as E
import qualified SLIR.HelmSyntax.AST.Data.Semantic.TermLevel.Patterns as P

-- ++ TopLevel
import qualified SLIR.HelmSyntax.AST.Data.Semantic.TopLevel.Fixities  as Decl
import qualified SLIR.HelmSyntax.AST.Data.Semantic.TopLevel.Functions as Decl
import qualified SLIR.HelmSyntax.AST.Data.Semantic.TopLevel.Unions    as Decl

-- + Local
import qualified SLIR.HelmSyntax.Module.Core.Parser.Syntax.Base.Metadata as Meta
import qualified SLIR.HelmSyntax.Module.Core.Parser.Syntax.Base.Ident    as ID
import qualified SLIR.HelmSyntax.Module.Core.Parser.Syntax.Base.Values   as V
import qualified SLIR.HelmSyntax.Module.Core.Parser.Syntax.Base.Types    as T
import qualified SLIR.HelmSyntax.Module.Core.Parser.Syntax.Base.Header   as Header
-- *





-- | Parse import declaration.
-- E.g.
-- * `import Sample.Lib`
-- * `import Sample.Lib as Lib`
-- * `import Sample.Lib exposing (x, y, z)`
-- * `import Sample.Lib as Lib exposing (x, y , z)`
--
parseImportDecl :: Parser Header.ImportDecl
parseImportDecl = do
    reservedWord "import"
    name             <- ID.parseNamespace
    asName           <- optional asName
    explicitExposing <- optional (try parseExplicitExposing)
    
    scn
    return
        $ Header.ImportDecl name asName explicitExposing




parseExplicitExposing :: Parser Header.Entries
parseExplicitExposing =
    L.lineFold scn $ \scn' -> do
        -- optional scn'
        reservedWord "exposing"
        -- optional scn'
        Header.parseEntries



-- *
-- | Internal Helpers
-- *


asName :: Parser ID.Ident
asName =
    reservedWord "as" *> ID.parseBig


















