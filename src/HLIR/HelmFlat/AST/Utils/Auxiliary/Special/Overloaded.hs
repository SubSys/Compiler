{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns #-}
module HLIR.HelmFlat.AST.Utils.Auxiliary.Special.Overloaded (
    group
  , separateOverloads
  , onlyNonOverloads
  , getAndMapOverloads
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

-- + Local
import qualified HLIR.HelmFlat.AST.Utils.Auxiliary.Ident as ID
-- *



type Overloaded = Decl.Function
type Normal = Decl.Function

-- |
-- NOTE:
-- * Functions with the same name will be grouped together,
--   therefore non-overloaded functions will be a sub-list
--   of length 1.
--
group :: [Decl.Function] -> [[Decl.Function]]
group =
    List.groupBy processPred


onlyNonOverloads :: [Decl.Function] -> [Decl.Function]
onlyNonOverloads = snd . separateOverloads

onlyOverloads :: [Decl.Function] -> [[Overloaded]]
onlyOverloads = fst . separateOverloads


getAndMapOverloads :: [Decl.Function] -> [(ID.Ident, [Decl.Function])]
getAndMapOverloads decls =
    let decls' = group decls
            |> List.filter (\x -> List.length x >= 2)

        idents = map extractName decls'

    in
        -- Map.fromList $ List.zip idents decls'
        List.zip idents decls'

    where
        extractName :: [Decl.Function] -> ID.Ident
        extractName fns =
            let
                -- NOTE: every ID should be the same...
                (x:xs) = map ID.get fns

            in
                x


separateOverloads :: [Decl.Function] -> ([[Overloaded]], [Normal])
separateOverloads decls =
    let decls' = group decls
            |> List.filter (\x -> List.length x >= 2)
        
        rest :: [Normal]
        rest = group decls
            |> List.filter (\x -> List.length x == 1)
            |> flatten
        
    in
        (decls', rest)





-- | Internal Helpers
--
processPred :: Decl.Function -> Decl.Function -> Bool
processPred
    (Decl.Function (Etc.Binder name1 _) _ _ (validSignature -> True) _)
    (Decl.Function (Etc.Binder name2 _) _ _ (validSignature -> True) _) =
        name1 == name2


processPred _ _ = False


validSignature :: Decl.Signature -> Bool
validSignature Decl.Validated{} = True
validSignature Decl.Unresolved{} = True
validSignature Decl.Unknown = False



