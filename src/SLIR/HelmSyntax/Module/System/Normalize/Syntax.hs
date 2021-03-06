{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
module SLIR.HelmSyntax.Module.System.Normalize.Syntax (
    normalize
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




{-# ANN module ("HLint: ignore" :: String) #-}





normalize :: (Data.Data a, Data.Typeable a) => I.Module -> a -> a
normalize payload = Uni.transformBi (normIdent payload)





-- normIdent :: I.Module -> ID.Ident -> ID.Ident
-- normIdent payload ident
--     | isLocal ident payload =
--         namespaceOverride (I.getModuleName payload) ident
-- 
-- 
--     | isExternal ident payload =
        -- case lookupOriginalNamespace ident of
        --     Nothing -> error $ Text.unpack $ Text.unlines
        --         [ Text.pack "Internal compiler failure!"
        --         , Text.pack "Failed to find the original namespace for:"
        --         , Text.pack $ PP.prettyShow ident
        --         , Text.pack "During normalization."
        --         , Text.pack "\n"
        --         , Text.pack "Current Module:"
        --         , Text.pack $ PP.prettyShow (I.getModuleName payload)
        --         ]
        --     Just ns ->
        --         namespaceOverride (I.getModuleName payload) ident
-- 
--     | otherwise =
--         ident


normIdent :: I.Module -> ID.Ident -> ID.Ident
normIdent payload ident
    | isLocal ident payload =
        namespaceOverride (I.getModuleName payload) ident
    
    | (Just depIdent) <- isExternal ident payload =
        case lookupOriginalNamespace depIdent of
                Nothing -> error $ Text.unpack $ Text.unlines
                    [ Text.pack "Internal compiler failure!"
                    , Text.pack "Failed to find the original namespace for:"
                    , Text.pack $ PP.prettyShow ident
                    , Text.pack "During normalization."
                    , Text.pack "\n"
                    , Text.pack "Current Module:"
                    , Text.pack $ PP.prettyShow (I.getModuleName payload)
                    ]
                Just ns ->
                    namespaceOverride ns ident
    
    | otherwise =
        ident




-- | Internal Helpers
--


isLocal :: ID.Ident -> I.Module -> Bool
isLocal name payload
    |  name `List.elem` ID.gets fns
    || name `List.elem` ID.gets cons
    || name `List.elem` ID.gets uns  = True
    | otherwise                      = False
    where
        uns = I.getUnions payload
        fns = I.getFunctions payload
        cons = flatten $ map Union.getConstructors uns

-- | 
-- I.e. a dependency
-- isExternal :: ID.Ident -> I.Module -> Bool
-- isExternal name payload
--     |  name `List.elem` ID.gets funDeps
--     || name `List.elem` ID.gets unsDeps 
--     || name `List.elem` ID.gets cons    = True
--     | otherwise                         = False
--     where
--         unsDeps = I.getUnionDeps payload
--         funDeps = I.getFunctionDeps payload
--         cons = flatten $ map Union.getConstructors unsDeps


namespaceOverride :: (Data.Data a, Data.Typeable a) => ID.Namespace -> a -> a
namespaceOverride ns = Uni.transformBi f
    where
        f :: Maybe ID.Namespace -> Maybe ID.Namespace
        f _ = Just ns



lookupOriginalNamespace :: ID.Ident -> Maybe ID.Namespace
lookupOriginalNamespace (ID.Ident _ _ meta) = Meta.originalNamespace meta



-- | 
-- I.e. a dependency
isExternal :: ID.Ident -> I.Module -> Maybe ID.Ident
isExternal name payload
    | Just x <- List.find (== name) (ID.gets funDeps) = Just x
    | Just x <- List.find (== name) (ID.gets unsDeps) = Just x
    | Just x <- List.find (== name) (ID.gets cons)    = Just x
    | otherwise                                       = Nothing
    where
        unsDeps = I.getUnionDeps payload
        funDeps = I.getFunctionDeps payload
        cons = flatten $ map Union.getConstructors unsDeps



