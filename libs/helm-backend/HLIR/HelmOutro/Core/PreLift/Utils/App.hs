{-# LANGUAGE NoImplicitPrelude #-}
module HLIR.HelmOutro.Core.PreLift.Utils.App where


-- *
import Core
import Core.Control.Flow ((|>), (<|))
import Prelude (return, String, IO, show, error, (<$>))

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
import qualified Text.Show.Prettyprint as PP


--- Local Deps
-- ~ HelmOutro AST
-- ~~ Base
import qualified HLIR.HelmOutro.AST.Base.Ident  as ID
import qualified HLIR.HelmOutro.AST.Base.Types  as T
import qualified HLIR.HelmOutro.AST.Base.Values as V
import qualified HLIR.HelmOutro.AST.Base.Etc    as Etc
-- ~~ TermLevel
import qualified HLIR.HelmOutro.AST.TermLevel.Expressions as E
import qualified HLIR.HelmOutro.AST.TermLevel.Patterns    as P
-- ~~ TopLevel
import qualified HLIR.HelmOutro.AST.TopLevel.Functions as Decl
import qualified HLIR.HelmOutro.AST.TopLevel.Unions    as Decl

--- Local
-- *



{-# ANN module "HLint: ignore" #-}




getCallee :: E.Expr -> E.Expr
getCallee (E.App e1 e2) = getCallee e1
getCallee x = x


flattenApps :: E.Expr -> [E.Expr]
flattenApps (E.App e1 e2) =
    flattenApps e1 ++ flattenApps e2

flattenApps x = [x]



-- | Get 'Callee' arity from type.
-- NOTE:
-- * Note, flatten types will include the output type as well.
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
        flatten (T.Arr t1 t2) =
            t1 : flatten t2
        
        flatten x = [x]



-- *
-- | Misc.
-- *

-- | Gen fresh Arg and Var pair

genFreshAVPair :: Int -> ([Etc.Arg], [E.Expr])
genFreshAVPair i =
    let args = genFreshArgs i
        vars = genFreshVars i
    in
        (args, vars)


-- *
-- | Internal
-- *
genFreshArgs :: Int -> [Etc.Arg]
genFreshArgs 0 = []
genFreshArgs i =
    newArg : genFreshArgs (i - 1)
    
    where
        prefix :: Text
        prefix = Text.pack "!"

        name :: Text
        name = 
            prefix `Text.append` Text.pack (show i)
        
        newBinder :: ID.Binder
        newBinder = ID.Binder name Nothing

        newArg :: Etc.Arg
        newArg =
            Etc.Arg newBinder Nothing


genFreshVars :: Int -> [E.Expr]
genFreshVars 0 = []
genFreshVars i =
    newVar : genFreshVars (i - 1)
    
    where
        prefix :: Text
        prefix = Text.pack "!"

        name :: Text
        name = 
            prefix `Text.append` Text.pack (show i)
        
        ref :: ID.Ref
        ref = ID.Ref name Nothing

        newVar :: E.Expr
        newVar =
            E.Var ref


