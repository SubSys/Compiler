{-# LANGUAGE NoImplicitPrelude #-}
module HLIR.HelmCore.Pass.Uncurry.InsertImplicitArguments.Helpers where


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

import qualified Data.List     as List
import qualified Data.Text     as Text
import qualified Data.Map      as Map
import qualified Data.Set      as Set
import qualified Data.Foldable as Fold
import qualified Data.Monoid   as Monoid

import qualified Data.Generics.Uniplate.Data as Uni


-- ~ Upstream
import qualified SLIR.HelmSyntax.Core as HelmSyntax

-- ~ HelmSyntax IR
import qualified HLIR.HelmCore.Data.Payload as Payload

--- Local Deps
-- ~ HelmCore AST
-- ~~ Base
import qualified HLIR.HelmCore.AST.Base.Ident  as ID
import qualified HLIR.HelmCore.AST.Base.Types  as T
import qualified HLIR.HelmCore.AST.Base.Values as V
-- ~~ TermLevel
import qualified HLIR.HelmCore.AST.TermLevel.Expressions as E
import qualified HLIR.HelmCore.AST.TermLevel.Patterns    as P
-- ~~ TopLevel
import qualified HLIR.HelmCore.AST.TopLevel.Functions as Decl
import qualified HLIR.HelmCore.AST.TopLevel.Unions    as Decl
-- *




genArgs :: Int -> [ID.Binder]
genArgs 0 = []
genArgs i
    | i < 0 =
        error "Int should be ‘non-negative’ number!"
    | otherwise =
        newArg (show i) : genArgs (i - 1)

    where
        newArg idx =
            let
                prefix = Text.pack "!"
                name   = Text.pack idx
            in
                ID.Binder (prefix `Text.append` name) Nothing


applyPartialArguments :: E.Expr -> [ID.Binder] -> E.Expr
applyPartialArguments e bs =
    let e' = List.foldl E.App e vars
    in
        List.foldr E.Abs e' bs

    where
        vars = map E.Var $ map binder2Ref bs

        binder2Ref :: ID.Binder -> ID.Ref
        binder2Ref (ID.Binder txt ns) =
            ID.Ref txt ns



-- | Return Number of implicit Args
--
hasImplicitArgs :: Decl.Function -> Maybe Int
hasImplicitArgs (Decl.Function name expr (T.Forall as t))
    | tsLength >= 2 =
        let (inputTs, outputType) = splitTypes ts
            inputTsLength = List.length inputTs

        in
            if inputTsLength == argsLength then
                Nothing 
            else if inputTsLength > argsLength then
                
                
                Just $ inputTsLength - argsLength
            
            else
                    Nothing
    
    | tsLength == 1 =
        Nothing


    where
        
        (args, expr') = splitArgs expr
        ts            = flattenTypes t
        
        argsLength = List.length args
        tsLength   = List.length ts


-- | flatten top-level types
-- E.g.
-- Input: `(a -> b) -> a -> b`
-- Output: `[(a -> b), a, b]`

flattenTypes :: T.Type -> [T.Type]
flattenTypes (T.Arr t1 t2) =
    t1 : flattenTypes t2
flattenTypes x = [x]


splitArgs :: E.Expr -> ([ID.Binder], E.Expr)
splitArgs (E.Abs b e) =
    let 
        (bs, e') = splitArgs e
    in
        (b : bs, e')

splitArgs x = ([], x)


-- | Split (flattened) types, separating input types from the output type.
--
splitTypes :: [T.Type] -> ([T.Type], T.Type)
splitTypes ts =
    let ins = List.init ts
        out = List.last ts
    in
        (ins, out)




