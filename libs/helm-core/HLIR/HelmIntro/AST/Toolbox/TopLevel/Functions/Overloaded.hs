{-# LANGUAGE NoImplicitPrelude #-}
module HLIR.HelmIntro.AST.Toolbox.TopLevel.Functions.Overloaded (
    group
) where


-- *
import Core
import Core.Control.Flow ((|>), (<|))
import Core.List.Util    (flatten)

import Prelude (return, String, IO, show, error, (<$>), (>>))

import Data.List.Index  (imap)

import qualified Control.Monad.State        as M
import qualified Control.Monad.Except       as M
import qualified Control.Monad.RWS          as M
import qualified Control.Monad.Identity     as M
import qualified Control.Monad.Reader       as M

import qualified Data.List     as List
import qualified Data.Text     as Text
import qualified Data.Map      as Map
import qualified Data.Set      as Set
import qualified Data.Foldable as Fold
import qualified Data.Monoid   as Monoid

import qualified Data.Generics.Uniplate.Data as Uni



--- Local Deps
-- ~ HelmSyntax AST
-- ~~ Base
import qualified HLIR.HelmIntro.AST.Data.Base.Etc    as Etc
import qualified HLIR.HelmIntro.AST.Data.Base.Ident  as ID
import qualified HLIR.HelmIntro.AST.Data.Base.Types  as T
import qualified HLIR.HelmIntro.AST.Data.Base.Values as V
import qualified HLIR.HelmIntro.AST.Data.Base.Metadata as Meta

-- ~~ TermLevel
import qualified HLIR.HelmIntro.AST.Data.TermLevel.Expressions as E
import qualified HLIR.HelmIntro.AST.Data.TermLevel.Patterns    as P

-- ~~ TopLevel
import qualified HLIR.HelmIntro.AST.Data.TopLevel.Fixities  as Decl
import qualified HLIR.HelmIntro.AST.Data.TopLevel.Functions as Decl
import qualified HLIR.HelmIntro.AST.Data.TopLevel.Unions    as Decl

--- Local
import qualified HLIR.HelmIntro.AST.Auxiliary.Canonical.Ident as CID
-- *


-- |
-- NOTE:
-- * Functions with the same name will be grouped together,
--   therefore non-overloaded functions will be a sub-list
--   of length 1.
--
group :: [Decl.Function] -> [[Decl.Function]]
group =
    List.groupBy processPred



-- # Overloaded difference (for determining the resulting constraints)
-- NOTE:
--    - E.g. 1
--            - `Int -> Int -> Int`
--            - `Float -> Float -> Float`
--    
--        - Denotes,
--            - `a -> a -> a` 
--    
--    - E.g.  2
--            - Int -> Int -> Bool
--            - Float -> Float -> Boo
--        - Denotes,
--        - `a -> a -> Bool`
--    
--    - Where (in the context of E.g. 1 and 2):
--        - `a` is the constraint that denotes either *all* Floats, or *all* Ints
--
-- NOTE:
-- * The scoped type variables denotes constraints.
-- ** E.g. `Int -> Int -> Bool` = `Forall [a] (a -> a -> Bool)`

-- TODO: ....
-- overloadedDiff :: [T.Type] -> Either OverloadedError  T.Scheme
-- overloadedDiff ts =
--     let ts1 = map flatten ts
-- 
--     in
--         T.Forall [] (T.Var' (ID.Low' (Text.pack "a") Nothing))
-- 
--     where
-- 
--         diff :: T.Type -> T.Type -> T.Type
--         diff t1 t2 =
--             if t1 == t2 then
--                 T.Var
-- 
--         flatten :: T.Type -> [T.Type]
--         flatten (T.Arr t1 t2 _) =
--             t1 : flatten t2
-- 
--         flatten x = [x]







-- *
-- | Misc. Data Types
-- *


-- TODO: ...
data OverloadedError
    = OverloadedError







-- *
-- | Internal Helpers
-- *

-- |
-- NOTE:
-- * All overloaded functions must have a type signature.
--
processPred :: Decl.Function -> Decl.Function -> Bool
processPred (Decl.FnDecl name1 _ _ (Just _) _) (Decl.FnDecl name2 _ _ (Just _) _) =
    name1 == name2

processPred (Decl.OpDecl name1 _ _ (Just _) _) (Decl.OpDecl name2 _ _ (Just _) _) =
    name1 == name2

processPred _ _ = False
