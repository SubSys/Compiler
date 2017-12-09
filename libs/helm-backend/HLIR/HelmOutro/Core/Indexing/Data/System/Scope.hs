{-# LANGUAGE NoImplicitPrelude #-}
module HLIR.HelmOutro.Core.Indexing.Data.System.Scope where


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
import qualified HLIR.HelmOutro.Core.Indexing.Data.Subst            as Sub
import qualified HLIR.HelmOutro.Core.Indexing.Data.System           as Sys
import qualified HLIR.HelmOutro.Core.Indexing.Data.System.Bindable  as Bind
import qualified HLIR.HelmOutro.Core.Indexing.Data.System.Referable as Ref

-- ~ Indexable Stuff
import HLIR.HelmOutro.Core.Indexing.Data.System (Index, Indexable(..), enter)
-- *



withLocalSubst :: Sub.Subst -> Sys.State a -> Sys.State a
withLocalSubst s1 =
    M.local modEnv
    
    where
        
        modEnv (a, s2) = (a, Sub.merge s1 s2)




-- localEnv :: Env.Env -> Sys.State a -> Sys.State a
-- localEnv env =
--     M.local modEnv
-- 
--     where
--         modEnv _ = env



-- -- | Lookup type in the environment
-- lookupEnv :: ID.Low -> Sys.State T.Type
-- lookupEnv name = do
--     env <- M.ask
-- 
--     case Env.lookup name env of
--         Nothing ->
--             M.throwError
--                 $ Report.Unbound
--                 $ Text.pack
--                 $ show name
-- 
--         Just s -> TS.instantiate s




