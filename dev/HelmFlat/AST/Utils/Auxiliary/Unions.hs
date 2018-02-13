{-# LANGUAGE NoImplicitPrelude #-}
module HLIR.HelmFlat.AST.Utils.Auxiliary.Unions (
    lookupUnion
  , lookupConstructor
  , lookupUnionConPair
  , getConstructors
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


-- + Recursion Schemes & Related
import qualified Data.Functor.Foldable as F

-- + Dev & Debugging
import qualified Text.Show.Prettyprint as PP

-- + HelmFlat Module Interface
import qualified HLIR.HelmFlat.Module.Data.Interface as I

-- + HelmFlat AST
-- ++ Base
import qualified HLIR.HelmFlat.AST.Data.Semantic.Base.Etc      as Etc
import qualified HLIR.HelmFlat.AST.Data.Semantic.Base.Ident    as ID
import qualified HLIR.HelmFlat.AST.Data.Semantic.Base.Types    as T
import qualified HLIR.HelmFlat.AST.Data.Semantic.Base.Values   as V
import qualified HLIR.HelmFlat.AST.Data.Semantic.Base.Metadata as Meta

-- ++ TermLevel
import qualified HLIR.HelmFlat.AST.Data.Semantic.TermLevel.Expr     as E
import qualified HLIR.HelmFlat.AST.Data.Semantic.TermLevel.Patterns as P

-- ++ TopLevel
import qualified HLIR.HelmFlat.AST.Data.Semantic.TopLevel.Fixities  as Decl
import qualified HLIR.HelmFlat.AST.Data.Semantic.TopLevel.Functions as Decl
import qualified HLIR.HelmFlat.AST.Data.Semantic.TopLevel.Unions    as Decl
-- *



-- {-# ANN module ("HLint: ignore" :: String) #-}


-- | Internal
type ConstructorName = ID.Ident


lookupUnion :: ConstructorName -> [Decl.Union] -> Maybe Decl.Union
lookupUnion name =
    List.find check
    where
        
        check union@(Decl.Union _ _ cs meta) =
            List.any checkInner cs
        
        checkInner (Decl.Constructor name' args meta)
            | name' == name = True
            | otherwise     = False



lookupConstructor :: ConstructorName -> Decl.Union -> Maybe Decl.Constructor
lookupConstructor name (Decl.Union _ _ cs _) =
    List.find check cs
    where
        check (Decl.Constructor name' args meta)
            | name' == name = True
            | otherwise     = False



lookupUnionConPair :: ConstructorName -> [Decl.Union] -> Maybe (Decl.Union, Decl.Constructor)
lookupUnionConPair name unions =
    case lookupUnion name unions of
        Nothing -> Nothing
        Just union -> case lookupConstructor name union of
            Nothing -> Nothing
            Just con ->
                Just (union, con)


getConstructors :: Decl.Union -> [Decl.Constructor]
getConstructors (Decl.Union _ _ cs _) = cs




