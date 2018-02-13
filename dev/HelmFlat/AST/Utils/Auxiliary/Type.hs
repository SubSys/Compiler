{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns #-}
module HLIR.HelmFlat.AST.Utils.Auxiliary.Type (
    getReturnType
  , getInputTypes
  , flatten
  , getArity
) where


-- *
import Core
import Core.Control.Flow ((|>), (<|))
-- import Core.List.Util
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
import qualified Data.Functor.Foldable       as F
import qualified Data.Generics.Uniplate.Data as Uni


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



getReturnType :: T.Type -> T.Type
getReturnType (T.Arr t1 t2 _) =
    getReturnType t2

getReturnType x = x





getInputTypes :: T.Type -> [T.Type]
getInputTypes (flatten -> ts)
    | List.length ts >= 2 =
        List.init ts
    
    | otherwise =
        []



flatten :: T.Type -> [T.Type]
flatten (T.Arr t1 t2 _) =
    t1 : flatten t2

flatten x = [x]



-- | Get 'Callee' arity from type.
-- NOTE:
-- * Note, flatten types will include the output type as well (So use this to get the 'args'...).
--   I.e. if it’s length is one then it doest take any values, and simply returns a single value.
--

getArity :: T.Type -> Int
getArity ty
    | length (flatten ty) == 1 = 0
    | length (flatten ty) == 2 = 1
    | otherwise =
        flatten ty |> List.init
                   |> List.length

    where
        length = List.length
        
        flatten :: T.Type -> [T.Type]
        flatten (T.Arr' t1 t2) =
            t1 : flatten t2
        
        flatten x = [x]











