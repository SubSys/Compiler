{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns #-}
module HLIR.HelmCore.Pass.Uncurry.InsertImplicitArguments (
      expandArguments
    , expandArguments'
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

--- Local
import qualified HLIR.HelmCore.Pass.Uncurry.InsertImplicitArguments.Helpers as Helper
-- *



-- *
-- Example Input:
-- º0 : forall a b. (a -> b) -> a -> b
-- º0 =
--     (λº1. (λº2. (º1 º2)))
-- 
-- º3 : forall a. a -> a
-- º3 =
--     (λº4. º4)
-- 
-- º5 : forall a. a -> a
-- º5 =
--     (º0 º3)
--
-- NOTE:
-- * From the above, `º5` calls `º0` with a
--   partially applied argument of `º3`.
--   This pass simply renders implicit arguments into explicit variants.
--   I.e. Inserting expected arguments into the AST.
--   E.g. `º5` becomes:
-- ```
--     AbsExpr (AbsScope
--                 (Binder "!1" Nothing)
--                 (AppExpr
--                     (AppExpr
--                         (VarExpr (Ref "º0" Nothing))
--                         (VarExpr (Ref "º3" Nothing)))
--                     (VarExpr (Ref "!1" Nothing))))
-- ```
-- *



expandArguments :: Decl.Function -> Decl.Function
expandArguments = Uni.transform expandArguments'



expandArguments' :: Decl.Function -> Decl.Function
expandArguments' fn@(Helper.hasImplicitArgs -> Just x) =
    expand fn

    where
        expand (Decl.Function name expr scheme) =
            let (args, expr1) = Helper.splitArgs expr
                endingArgs    = Helper.genArgs x
                
                expr2 = Helper.applyPartialArguments expr1 (args ++ endingArgs)
            in
                Decl.Function name expr2 scheme


expandArguments' x = x







