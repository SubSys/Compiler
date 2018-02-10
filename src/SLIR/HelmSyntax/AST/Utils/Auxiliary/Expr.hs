{-# LANGUAGE NoImplicitPrelude #-}
module SLIR.HelmSyntax.AST.Utils.Auxiliary.Expr (
    freeVars
  , getCallee
  , getConstrCallee
  , flattenApps
  , freshVar
  , hoistLambdas
) where


-- *
import Core
import Core.Control.Flow ((|>), (<|))
import Core.List.Util (flatten, singleton)
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

-- + HelmSyntax Module Interface
import qualified SLIR.HelmSyntax.Module.Data.Interface as I

-- + HelmSyntax AST Utils
import qualified SLIR.HelmSyntax.AST.Utils.Auxiliary.Ident             as ID
import qualified SLIR.HelmSyntax.AST.Utils.Auxiliary.Functions.SudoFFI as SudoFFI

-- + HelmSyntax AST
-- ++ Base
import qualified SLIR.HelmSyntax.AST.Data.Semantic.Base.Etc      as Etc
import qualified SLIR.HelmSyntax.AST.Data.Semantic.Base.Ident    as ID
import qualified SLIR.HelmSyntax.AST.Data.Semantic.Base.Types    as T
import qualified SLIR.HelmSyntax.AST.Data.Semantic.Base.Values   as V
import qualified SLIR.HelmSyntax.AST.Data.Semantic.Base.Metadata as Meta

-- ++ TermLevel
import qualified SLIR.HelmSyntax.AST.Data.Semantic.TermLevel.Expr     as E
import qualified SLIR.HelmSyntax.AST.Data.Semantic.TermLevel.Patterns as P

-- ++ TopLevel
import qualified SLIR.HelmSyntax.AST.Data.Semantic.TopLevel.Fixities  as Decl
import qualified SLIR.HelmSyntax.AST.Data.Semantic.TopLevel.Functions as Decl
import qualified SLIR.HelmSyntax.AST.Data.Semantic.TopLevel.Unions    as Decl
-- *





freeVars :: E.Expr -> [ID.Ident]
freeVars input =
    let
        binders = [ x | (Etc.Binder x ty) <- Uni.universeBi input]
        vars1    = [ x | (E.Var x meta) <- Uni.universe input]
        vars2    = [ x | (E.InfixApp x _ _ _) <- Uni.universe input]

    in
        (vars1 ++ vars2) `without` binders
            |> List.filter (not . sudoFFI)

    where
        without :: Eq a => [a] -> [a] -> [a]
        without =
            Fold.foldr (List.filter . (/=))
        
        
        sudoFFI :: ID.Ident -> Bool
        sudoFFI (ID.Ident _ ns _) = SudoFFI.isSudoNS ns



-- getCallee :: E.Expr -> E.Expr
-- getCallee (E.App' e1 e2) = getCallee e1
-- getCallee x = x


getCallee :: E.Expr -> Maybe ID.Ident
getCallee (E.App' e1 _) = getCallee e1

getCallee (E.Var' ident) = Just ident

getCallee _ = Nothing



getConstrCallee :: E.Expr -> Maybe ID.Ident
getConstrCallee (E.App' e1 _) = getConstrCallee e1

getConstrCallee (E.Constr' ident) = Just ident

getConstrCallee _ = Nothing



flattenApps :: E.Expr -> [E.Expr]
flattenApps (E.App' e1 e2) =
    flattenApps e1 ++ flattenApps e2

flattenApps x = [x]





freshVar :: Int -> E.Expr
freshVar idx =
    newVar
    where
        prefix :: Text
        prefix = Text.pack "!"
        
        name :: Text
        name = 
            prefix `Text.append` Text.pack (Pre.show idx)

        newVar :: E.Expr
        newVar =
            E.Var' $ ID.Ident_ name



-- | Hoist (immediate) repeated sequences of nested abstractions
--
-- Continually build up a list of nested function abstractions,
-- or lambdas until another expression constructor is encountered.
--
hoistLambdas :: E.Expr -> ([Etc.Binder], E.Expr)
hoistLambdas (E.Abs' b e) =
    let (bs', e') = hoistLambdas e
    in
        (b : bs', e')

hoistLambdas x = ([], x)







