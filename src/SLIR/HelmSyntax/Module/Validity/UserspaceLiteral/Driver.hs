{-# LANGUAGE NoImplicitPrelude #-}
-- {-# LANGUAGE PatternGuards #-}
module SLIR.HelmSyntax.Module.Validity.UserspaceLiteral.Driver (
    userspaceLiteral
  , userspaceLiteral'
) where


-- *
import Core
import Core.Control.Flow ((|>), (<|))
import Core.List.Util    (flatten, singleton)
import Data.Monoid ((<>))
import Prelude
    ( return
    , String
    , IO
    , show
    , error
    , (<$>)
    , (>>=)
    , (>>)
    , fromIntegral
    )


import qualified Prelude    as Pre
import qualified Core.Utils as Core

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
import qualified Data.Data                    as Data


-- + Recursion Schemes & Related
import qualified Data.Functor.Foldable       as F
import qualified Data.Generics.Uniplate.Data as Uni


-- + Dev & Debugging
import qualified Text.Show.Prettyprint as PP

-- + HelmSyntax Module Interface
import qualified SLIR.HelmSyntax.Module.Data.Interface as I

-- + HelmSyntax AST Utils
import qualified SLIR.HelmSyntax.AST.Utils.Scope                       as Scope
import qualified SLIR.HelmSyntax.AST.Utils.Auxiliary.Ident             as ID
import qualified SLIR.HelmSyntax.AST.Utils.Auxiliary.Functions.SudoFFI as SudoFFI
import qualified SLIR.HelmSyntax.AST.Utils.Auxiliary.Unions            as Union

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
-- *


userspaceLiteral :: IO (Either Text I.Module) -> IO (Either Text I.Module)
userspaceLiteral upstream = do
    result <- upstream
    
    case result of
        Left err -> return $ Left err
        Right payload ->
            return
                $ userspaceLiteral' payload
    

userspaceLiteral' :: I.Module -> Either Text I.Module
userspaceLiteral' payload
    | Just ty <- List.find (`List.elem` literalTypes) (ID.getTexts $ ID.gets uns) =
        Left $ Text.unlines
            [ Text.pack "You’ve defined a type that I have already defined:"
            , Text.pack $ PP.prettyShow ty 
            , Text.pack "Rejecting due to possibility of weird interactions."
            ]
    | Just c <- List.find (`List.elem` literalConstrValues) (ID.getTexts $ ID.gets cons) =
        Left $ Text.unlines
            [ Text.pack "You’ve defined a value that I have already defined:"
            , Text.pack $ PP.prettyShow c
            , Text.pack "Rejecting due to possibility of weird interactions."
            ]
    | otherwise = Right payload
    where
        uns  = I.getUnions payload
        cons = flatten $ map Union.getConstructors uns



literalTypes =
    [ Text.pack "String"
    , Text.pack "Char"
    , Text.pack "Bool"
    , Text.pack "Int"
    , Text.pack "Float"
    , Text.pack "I8"
    , Text.pack "I16"
    , Text.pack "I32"
    , Text.pack "I64"
    , Text.pack "I128"
    , Text.pack "U8"
    , Text.pack "U16"
    , Text.pack "U32"
    , Text.pack "U64"
    , Text.pack "U128"
    , Text.pack "F32"
    , Text.pack "F64"
    ]

literalConstrValues =
    [ Text.pack "True"
    , Text.pack "False"
    ]

