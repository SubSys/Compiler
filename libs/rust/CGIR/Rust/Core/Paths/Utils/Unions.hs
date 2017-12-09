{-# LANGUAGE NoImplicitPrelude #-}
module CGIR.Rust.Core.Paths.Utils.Unions (
      lookupUnion
    , lookupUnionName
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
-- *






-- *
-- | Paths
-- *



-- | Internal
type ConstructorName = ID.Big



lookupUnionName :: ConstructorName -> [Decl.Union] -> Maybe ID.Big
lookupUnionName con uns =
    case List.find check uns of
        Nothing -> Nothing
        Just (Decl.Union name' _ _) -> Just name'
    where

        check union@(Decl.Union _ _ cs) =
            List.any checkInner cs

        checkInner (Decl.Constructor name' args)
            | name' == con = True
            | otherwise     = False



lookupUnion :: ConstructorName -> [Decl.Union] -> Maybe Decl.Union
lookupUnion name  =
    List.find check
    where
        
        check union@(Decl.Union _ _ cs) =
            List.any checkInner cs
        
        checkInner (Decl.Constructor name' args)
            | name' == name = True
            | otherwise     = False



lookupConstructor :: ConstructorName -> Decl.Union -> Maybe Decl.Constructor
lookupConstructor name (Decl.Union _ _ cs) =
    List.find check cs
    where
        check (Decl.Constructor name' args)
            | name' == name = True
            | otherwise     = False



lookupUnionCon :: ConstructorName -> [Decl.Union] -> Maybe (Decl.Union, Decl.Constructor)
lookupUnionCon name unions =
    case lookupUnion name unions of
        Nothing -> Nothing
        Just union -> case lookupConstructor name union of
            Nothing -> Nothing
            Just con ->
                Just (union, con)








