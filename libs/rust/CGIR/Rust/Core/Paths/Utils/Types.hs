{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns #-}
module CGIR.Rust.Core.Paths.Utils.Types (
      initTypesEnv
    , isFnType
    , lookupRef
) where


-- *
import Core
import Core.Control.Flow ((|>), (<|))
import Prelude (return, String, IO, show, error, (<$>))

import Data.List.Index  (imap)

import qualified Control.Monad.State        as M
import qualified Control.Monad.Except       as M
import qualified Control.Monad.RWS          as M
import qualified Control.Monad.Identity     as M
import qualified Control.Monad.Reader       as M
import qualified Control.Monad.Writer       as M

import qualified Data.List     as List
import qualified Data.Text     as Text
import qualified Data.Map      as Map
import qualified Data.Set      as Set
import qualified Data.Foldable as Fold
import qualified Data.Monoid   as Monoid

import qualified Data.Generics.Uniplate.Data as Uni


--- Dev
import qualified Dev.Samples.Basic      as BasicSamples
import qualified Dev.Samples.Complex    as ComplexSamples
import qualified Dev.Samples.TestParser as ParserSample

import qualified Text.Show.Prettyprint as PP

import qualified CGIR.Rust.Render.Syntax as Display

-- ~ Upstream
import qualified SLIR.HelmSyntax.Core  as HelmSyntax
import qualified HLIR.HelmCore.Core    as HelmCore
import qualified HLIR.HelmOutro.Core   as HelmOutro

import qualified LLIR.LightRoast.Core as LightRoast



--- Local Deps
-- ~ LightRoast Payload
import qualified CGIR.Rust.Data.Payload as Payload

-- ~ (GCIR) - Rust AST
-- ~~ Base
import qualified CGIR.Rust.AST.Base.Ident  as ID
import qualified CGIR.Rust.AST.Base.Types  as T
import qualified CGIR.Rust.AST.Base.Values as V
import qualified CGIR.Rust.AST.Base.Etc    as Etc
-- ~~ TermLevel
import qualified CGIR.Rust.AST.TermLevel.Stmt        as S
import qualified CGIR.Rust.AST.TermLevel.Patterns    as P
import qualified CGIR.Rust.AST.TermLevel.Block       as Decl
-- ~~ TopLevel
import qualified CGIR.Rust.AST.TopLevel.Functions as Decl
import qualified CGIR.Rust.AST.TopLevel.Unions    as Decl


--- Local
import qualified CGIR.Rust.Core.Paths.Data.TypesEnv as TypesEnv
-- *




initTypesEnv :: [Decl.Function] -> TypesEnv.TypesEnv
initTypesEnv fns =
        TypesEnv.TypesEnv
            { TypesEnv.fnTypes = ingestDecls fns
            }


isFnType :: T.Type -> Bool
isFnType T.Fn{} = True
isFnType _      = False


lookupRef :: ID.Low -> TypesEnv.TypesEnv -> Bool
lookupRef name (TypesEnv.TypesEnv fnTs) =
    case Map.lookup name fnTs of
        Nothing -> False
        Just True -> True
        Just False -> False

-- *
-- | Internal Helpers
-- *

ingestDecls :: [Decl.Function] -> Map.Map ID.Low Bool
ingestDecls xs =
    let env1 = Fold.foldl' Map.union Map.empty [env | (ingestDecl -> env) <- Uni.universeBi xs]
        env2 = Fold.foldl' Map.union Map.empty [env | (ingestInput -> env) <- Uni.universeBi xs]
    in
        Map.union env1 env2


ingestDecl :: Decl.Function -> Map.Map ID.Low Bool
ingestDecl (Decl.Function name _ args (Etc.Output ty) _) =
    Map.singleton name True


ingestInput :: Etc.Input -> Map.Map ID.Low Bool
ingestInput (Etc.Input name T.Fn{}) =
    Map.singleton name True

ingestInput (Etc.Input name ty) =
    Map.singleton name False

ingestInput x = error errorMsg


errorMsg =
    "Internal compiler failure. `ingestInput` expects all binders to be annotated with it’s type!"








