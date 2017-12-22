{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module SLIR.HelmSyntax.AST.Instances.Hashable where


-- *
import Core

import qualified Data.Text as Text
import qualified Data.List as List

import qualified Data.Hashable as Hash
import qualified Data.HashMap.Strict as HashMap

-- import Data.Monoid ((<>))
-- import Data.Semigroup ((<>))

--- Local
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

-- ~~ Header
import qualified SLIR.HelmSyntax.AST.Data.Header.Base       as Base
import qualified SLIR.HelmSyntax.AST.Data.Header.ImportDecl as Decl
-- *








-- *
-- | # TopLevel
-- *



-- *
-- | ## Fixities
-- *





-- *
-- | ## Functions
-- *
instance Hash.Hashable Decl.Function where
    hashWithSalt i (Decl.FnDecl name args expr sig meta) =
        Hash.hashWithSalt i (name, args, expr, sig)
    
    hashWithSalt i (Decl.OpDecl name args expr sig meta) =
        Hash.hashWithSalt i (name, args, expr, sig)


-- *
-- | ## Unions
-- *







-- *
-- | # TermLevel
-- *





-- *
-- | ## Expressions
-- *


instance Hash.Hashable E.Expr where
    hashWithSalt i (E.Var name meta) =
        Hash.hashWithSalt i name

    hashWithSalt i (E.Lit val meta) =
        Hash.hashWithSalt i val

    hashWithSalt i (E.Record fields meta) =
        Hash.hashWithSalt i fields

    hashWithSalt i (E.Tuple items meta) =
        Hash.hashWithSalt i items

    hashWithSalt i (E.List xs meta) =
        Hash.hashWithSalt i xs

    hashWithSalt i (E.Con id' meta) =
        Hash.hashWithSalt i id'

    hashWithSalt i (E.BinOp sym e1 e2 meta) =
        Hash.hashWithSalt i (sym, e1, e2)

    hashWithSalt i (E.If intros elseExpr meta) =
        Hash.hashWithSalt i (intros, elseExpr)

    hashWithSalt i (E.Let fns expr meta) =
        Hash.hashWithSalt i (fns, expr)

    hashWithSalt i (E.Case con alts meta) =
        Hash.hashWithSalt i (con, alts)

    hashWithSalt i (E.RecordUpdate var fields meta) =
        Hash.hashWithSalt i (var, fields)

    hashWithSalt i (E.RecordAccess field object meta) =
        Hash.hashWithSalt i (field, object)

    hashWithSalt i (E.Parens expr meta) =
        Hash.hashWithSalt i expr

    hashWithSalt i (E.App e1 e2 meta) =
        Hash.hashWithSalt i (e1, e2)

    hashWithSalt i (E.Abs arg expr meta) =
        Hash.hashWithSalt i (arg, expr)

    hashWithSalt i (E.AltAbs args expr scheme) =
        Hash.hashWithSalt i (args, expr, scheme)










-- *
-- | ## Patterns
-- *

instance Hash.Hashable P.CaseAlt where
    hashWithSalt i (P.CaseAlt patrn expr meta) =
        Hash.hashWithSalt i (patrn, expr)

instance Hash.Hashable P.Pattern where
    hashWithSalt i (P.Lit lit meta) =
        Hash.hashWithSalt i lit

    hashWithSalt i (P.Record vars meta) =
        Hash.hashWithSalt i vars

    hashWithSalt i (P.List xs meta) =
        Hash.hashWithSalt i xs

    hashWithSalt i (P.Cons xs rest meta) =
        Hash.hashWithSalt i (xs, rest)

    hashWithSalt i (P.Tuple items meta) =
        Hash.hashWithSalt i items

    hashWithSalt i (P.Con id' args meta) =
        Hash.hashWithSalt i (id', args)

    hashWithSalt i (P.Var id' meta) =
        Hash.hashWithSalt i id'

    hashWithSalt i (P.Wildcard meta) =
        Hash.hashWithSalt i "Wildcard"







-- *
-- | # Base
-- *



-- *
-- | ## Identifiers (Ident)
-- *


instance Hash.Hashable ID.Namespace where
    hashWithSalt i (ID.Namespace segs) =
        Hash.hashWithSalt i segs


instance Hash.Hashable ID.Low where
    hashWithSalt i (ID.Low txt ns _) =
        Hash.hashWithSalt i (txt, ns)

instance Hash.Hashable ID.Big where
    hashWithSalt i (ID.Big txt ns _) =
        Hash.hashWithSalt i (txt, ns)


instance Hash.Hashable ID.Sym where
    hashWithSalt i (ID.Sym txt ns _) =
        Hash.hashWithSalt i (txt, ns)


-- *
-- | ## Types
-- *


instance Hash.Hashable T.Type where
    hashWithSalt i (T.Record fields meta) =
        Hash.hashWithSalt i fields

    hashWithSalt i (T.Tuple ts meta) =
            Hash.hashWithSalt i ts


    hashWithSalt i (T.List ty meta) =
        Hash.hashWithSalt i ty


    hashWithSalt i (T.Union name args meta) =
        Hash.hashWithSalt i (name, args)


    hashWithSalt i (T.Var id' meta) =
        Hash.hashWithSalt i id'


    hashWithSalt i (T.Arr ty1 ty2 meta) =
        Hash.hashWithSalt i (ty1, ty2)


    hashWithSalt i (T.Parens ty meta) =
        Hash.hashWithSalt i ty


    hashWithSalt i (T.String meta) =
        Hash.hashWithSalt i "String"


    hashWithSalt i (T.Char meta) =
        Hash.hashWithSalt i "Char"


    hashWithSalt i (T.Int meta) =
        Hash.hashWithSalt i "Int"


    hashWithSalt i (T.Float meta) =
        Hash.hashWithSalt i "Float"


    hashWithSalt i (T.Bool meta) =
        Hash.hashWithSalt i "Bool"


    hashWithSalt i (T.Superposed con ts) =
        Hash.hashWithSalt i (con, ts)




-- *
-- | ### Type Schemes
-- *

instance Hash.Hashable T.Scheme where
    hashWithSalt i (T.Forall as ty) =
        Hash.hashWithSalt i (as, ty)










-- *
-- | ## Values
-- *


instance Hash.Hashable V.LiteralValue where
    hashWithSalt i (V.Char val meta) =
        Hash.hashWithSalt i val

    hashWithSalt i (V.String val meta) =
        Hash.hashWithSalt i val

    hashWithSalt i (V.Int val meta) =
        Hash.hashWithSalt i val

    hashWithSalt i (V.Float val meta) =
        Hash.hashWithSalt i val

    hashWithSalt i (V.Bool val meta) =
        Hash.hashWithSalt i val






-- *
-- | Etc.
-- *
instance Hash.Hashable Etc.Signature where
    hashWithSalt i (Etc.Validated scheme meta) =
        Hash.hashWithSalt i scheme
    
    hashWithSalt i (Etc.Unresolved ty meta) =
        Hash.hashWithSalt i ty



-- *
-- | # Header
-- *


-- *
-- | ## Header Base
-- *


-- *
-- | ## Header Import Declarations
-- *



-- *
-- | Metadata, 'Source Code Location' & Related
-- *








