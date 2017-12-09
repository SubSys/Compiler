{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module SLIR.HelmSyntax.AST.Toolbox.Format.NoMeta (
    noMeta
) where


-- *
import Core
import Core.Control.Flow

import qualified Data.Text as Text
import qualified Data.List as List

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
-- *


class NoMeta a where
    noMeta :: a -> a

instance NoMeta Decl.Function where
    noMeta (Decl.FnDecl name args expr optSig _) =
        Decl.FnDecl
            (noMeta name)
            (map noMeta args)
            (noMeta expr)
            (noMeta optSig)
            Nothing

instance NoMeta E.Expr where
    noMeta (E.Let fns expr _) =
        E.Let (map noMeta fns) (noMeta expr) Nothing
    
    noMeta (E.Var name _) =
        E.Var name Nothing

    noMeta (E.Lit x _) =
        E.Lit (noMeta x) Nothing

    noMeta (E.Record fields _) =
        E.Record fields' Nothing
        where
            fields' = map field fields
            field (n, e) =
                (noMeta n, noMeta e)

    noMeta (E.Tuple items _) =
        E.Tuple (map noMeta items) Nothing

    noMeta (E.List xs _) =
        E.List (map noMeta xs) Nothing

    noMeta (E.Con x _) =
        E.Con (noMeta x) Nothing

    noMeta (E.BinOp sym e1 e2 _) =
        E.BinOp
            (noMeta sym)
            (noMeta e1)
            (noMeta e2)
            Nothing

    noMeta (E.If intros elseE _) =
        E.If intros' elseE' Nothing
        where
            elseE' = noMeta elseE
            intros' = map branch intros
            branch (con, body) =
                (noMeta con, noMeta body)

    -- noMeta (E.Case x _) =
    --     E.Case x Nothing

    -- noMeta (E.RecordUpdate x _) =
    --     E.RecordUpdate x Nothing
    -- 
    -- noMeta (E.RecordAccess x _) =
    --     E.RecordAccess x Nothing

    noMeta (E.Parens x _) =
        E.Parens (noMeta x) Nothing

    noMeta (E.App e1 e2 _) =
        E.App (noMeta e1) (noMeta e2) Nothing

    noMeta (E.Abs arg expr _) =
        E.Abs
            (noMeta arg)
            (noMeta expr)
            Nothing



instance NoMeta V.LiteralValue where
    noMeta (V.Char val _) =
        V.Char val Nothing

    noMeta (V.String val _) =
        V.String val Nothing

    noMeta (V.Int val _) =
        V.Int val Nothing

    noMeta (V.Float val _) =
        V.Float val Nothing

    noMeta (V.Bool val _) =
        V.Bool val Nothing



instance NoMeta ID.Big where
    noMeta (ID.Big txt ns _) =
        ID.Big txt ns Nothing

instance NoMeta ID.Low where
    noMeta (ID.Low txt ns _) =
        ID.Low txt ns Nothing

instance NoMeta ID.Sym where
    noMeta (ID.Sym txt ns _) =
        ID.Sym txt ns Nothing


instance NoMeta (Maybe Etc.Signature) where
    noMeta Nothing = Nothing
    noMeta (Just (Etc.Unresolved ty _)) =
        Just $ Etc.Unresolved (noMeta ty) Nothing


instance NoMeta T.Type where
    noMeta (T.Record fields _) =
        T.Record fields' Nothing
        where
            fields' = map field fields
            field (n, ty) =
                (noMeta n, noMeta ty)

    noMeta (T.Tuple items _) =
        T.Tuple (map noMeta items) Nothing

    noMeta (T.List ty _) =
        T.List (noMeta ty) Nothing

    noMeta (T.Union name args _) =
        T.Union (noMeta name) (map noMeta args) Nothing

    noMeta (T.Var x _) =
        T.Var (noMeta x) Nothing

    noMeta (T.Arr t1 t2 _) =
        T.Arr (noMeta t1) (noMeta t2) Nothing

    noMeta (T.Parens x _) =
        T.Parens (noMeta x) Nothing

    noMeta (T.String _) =
        T.String Nothing

    noMeta (T.Char _) =
        T.Char Nothing

    noMeta (T.Int _) =
        T.Int Nothing

    noMeta (T.Float _) =
        T.Float Nothing

    noMeta (T.Bool _) =
        T.Bool Nothing

