{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
module SLIR.HelmSyntax.Core.Program.SDD.Inference.Utils.Syntax.Scope (
    lookupEnv
  , lookupEnv'
  , withLocalBinder
  , withLocalEnv
  , isOverloaded
) where


-- *
import Core
import Core.Control.Flow ((|>), (<|))
import Core.List.Util    (flatten)
import Data.Data (Data, Typeable)

import Prelude (return, String, IO, show, error, (<$>), (>>))

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
import qualified Data.String   as String

import qualified Data.Generics.Uniplate.Data as Uni
import qualified Text.Show.Prettyprint as PP


-- ~ HelmSyntax Cores

-- ~ HelmSyntax IR
import qualified SLIR.HelmSyntax.Data.Interface.Program.Payload as Payload

--- Local Deps
-- ~ HelmSyntax AST
-- ~~ Base
import qualified SLIR.HelmSyntax.AST.Data.Base.Etc      as Etc
import qualified SLIR.HelmSyntax.AST.Data.Base.Ident    as ID
import qualified SLIR.HelmSyntax.AST.Data.Base.Types    as T
import qualified SLIR.HelmSyntax.AST.Data.Base.Values   as V
import qualified SLIR.HelmSyntax.AST.Data.Base.Metadata as Meta

-- ~~ TermLevel
import qualified SLIR.HelmSyntax.AST.Data.TermLevel.Expressions as E
import qualified SLIR.HelmSyntax.AST.Data.TermLevel.Patterns    as P

-- ~~ TopLevel
import qualified SLIR.HelmSyntax.AST.Data.TopLevel.Fixities  as Decl
import qualified SLIR.HelmSyntax.AST.Data.TopLevel.Functions as Decl
import qualified SLIR.HelmSyntax.AST.Data.TopLevel.Unions    as Decl

-- ~~ Auxiliary Nodes
import qualified SLIR.HelmSyntax.AST.Auxiliary.Canonical.Ident as CID


--- Local
import qualified SLIR.HelmSyntax.Core.Program.SDD.Inference.Data.Env         as Env
import qualified SLIR.HelmSyntax.Core.Program.SDD.Solver.Data.Constraint     as Con
import qualified SLIR.HelmSyntax.Core.Program.SDD.Data.Report                as Report
import qualified SLIR.HelmSyntax.Core.Program.SDD.Inference.Data.System      as Sys
import qualified SLIR.HelmSyntax.Core.Program.SDD.Inference.Utils.TypeSystem as TS
import qualified SLIR.HelmSyntax.Core.Program.SDD.Inference.Utils.ExprSystem as ES
import qualified SLIR.HelmSyntax.Core.Program.SDD.Inference.Utils.Overloaded as Overloaded
import qualified SLIR.HelmSyntax.Core.Program.SDD.Resolve.Engine             as Resolve
-- *




{-# ANN module ("HLint: ignore" :: String) #-}



-- | Extend type environment
--
withLocalBinder :: (ID.Low, T.Scheme) -> Sys.Infer a -> Sys.Infer a
withLocalBinder (x, sc) =
    M.local modEnv

    where
        key = CID.ident x
        value = Env.Normal sc

        modEnv e = (Map.delete key e `Map.union` (Map.singleton key value))



withLocalEnv :: Env.ExprEnv -> Sys.Infer a -> Sys.Infer a
withLocalEnv env1 =
    M.local modEnv

    where
        modEnv env2 = Map.union env1 env2



lookupEnv :: CID.Identifiable a => (E.Expr -> Sys.Syntax E.Expr) -> a -> Sys.Infer T.Type
lookupEnv f name = do
    exprEnv <- M.ask

    case Map.lookup (CID.ident name) exprEnv of
        Nothing ->
            M.throwError
                $ Report.UnboundVariable
                $ Text.pack
                $ PP.prettyShow
                $ CID.ident name
        
        Just entry -> case entry of
            Env.Normal s -> TS.instantiate s
            
            (Env.Superposed s expr) -> do
                env <- M.ask
                
                -- *
                t1 <- TS.instantiate s
                -- *
                
                -- *
                -- superTv <- TS.freshType
                -- let t2 = T.Superposed superTv []
                -- *
                
                
                -- *
                M.tell [Con.TypeCon t1 t1]
                -- *
                
                

                -- *
                (expr_, ty_, _) <- f expr
                M.tell [Con.TypeCon t1 ty_]
                -- *
                
                -- *
                M.tell [Con.Inline (CID.ident name) t1 expr_]
                -- *
                
                case expr_ of
                    E.AltAbs args body (Just scheme) -> do
                        t2 <- TS.instantiate scheme
                        -- *
                        
                        -- *
                        M.tell [Con.TypeCon ty_ t2]
                        M.tell [Con.TypeCon t1 t2]
                        -- *
                        
                        
                        return t2
                        
                        
                        
                    
                    _ -> return ty_
                
            
            (Env.Overloaded xs) -> do
                ts <- M.mapM TS.instantiate (Map.keys xs)
                
                t <- Overloaded.genOverloaded ts
                -- *
                
                
                -- *
                M.tell [Con.Overloaded (CID.ident name) t xs]
                -- *
                
                
                -- *
                return t



lookupEnv' :: CID.Identifiable a => a -> Sys.Infer T.Type
lookupEnv' name = do
    exprEnv <- M.ask

    case Map.lookup (CID.ident name) exprEnv of
        Nothing ->
            M.throwError
                $ Report.UnboundVariable
                $ Text.pack
                $ PP.prettyShow
                $ CID.ident name
        
        Just entry -> case entry of
            Env.Normal s -> TS.instantiate s




isOverloaded :: CID.Identifiable a => a -> Sys.Infer (Maybe [(T.Scheme, E.Expr)])
isOverloaded x = do
    env <- M.ask

    case Map.lookup (CID.ident x) env of
        Just entry -> case entry of
                Env.Overloaded xs ->
                    return $ Just $ Map.toList xs
                
                
                _ -> return Nothing
        _ -> return Nothing
    


