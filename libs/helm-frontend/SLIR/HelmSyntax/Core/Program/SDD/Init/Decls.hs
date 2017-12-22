{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
module SLIR.HelmSyntax.Core.Program.SDD.Init.Decls (
    initDecls
) where


-- *
import Core
import Core.Control.Flow ((|>), (<|))
import Core.List.Util    (flatten)
import Data.Data (Data, Typeable)

import Prelude (return, String, IO, show, error, (<$>), (>>))

import Data.List ((\\))

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
import qualified SLIR.HelmSyntax.Core.Program.SDD.Init.Decls.Overloads       as Overloads
import qualified SLIR.HelmSyntax.Core.Program.SDD.Init.Decls.Superposed      as Superposed
import qualified SLIR.HelmSyntax.Core.Program.SDD.Inference.Data.Env         as Env
import qualified SLIR.HelmSyntax.Core.Program.SDD.Inference.Utils.ExprSystem as ES
-- *


type Normals   = [Decl.Function]
type Overloads = [[Decl.Function]]
type Supers    = [Decl.Function]


initDecls :: [Decl.Function] -> (Normals, Env.ExprEnv)
initDecls decls0 =
    let
        (decls1, overloads, supers) = initDecls' decls0
        env = initOverloadedEnv overloads `Map.union` initSuperEnv supers
    in
        (decls1, env)


initDecls' :: [Decl.Function] -> (Normals, Overloads, Supers)
initDecls' decls =
    let
        (decls1, overloads) = Overloads.partitionDecls decls
        (decls2, supers)    = Superposed.partitionDecls decls1
    in
        (decls2, overloads, supers)



initSuperEnv :: Supers -> Env.ExprEnv
initSuperEnv [] = Map.empty

initSuperEnv (decl:rest) =
    let
        name = CID.ident decl
        entry =
            Env.Superposed (getScheme decl) (ES.instantiate' decl)
    in
        Map.insert name entry (initSuperEnv rest)



initOverloadedEnv :: Overloads -> Env.ExprEnv
initOverloadedEnv [] = Map.empty
initOverloadedEnv (group:rest)
    | not (null (ss1 \\ ss2)) =
        error "All overloaded functions must have a unique type signature."
        
    | not (null group) =
        let
            name = CID.ident $ List.head group
            items = Env.Overloaded $ Map.unions $ map initItem group
        in
            Map.insert name items (initOverloadedEnv rest)

    
    where
        ss1 = map getScheme group
        ss2 = Set.toList $ Set.fromList ss1
        
        
        initItem decl =
            Map.singleton (getScheme decl) (ES.instantiate decl)




-- add2Env (Overloads, Supers) -> Env.ExprEnv
-- add2Env (overloads, supers) =
--     where
--         toSuperEntry :: Env.Entry
--         toSuperEntry



getScheme :: Decl.Function -> T.Scheme
getScheme (Decl.FnDecl _ _ _ (Just (Etc.Validated scheme _)) _) = scheme
getScheme (Decl.OpDecl _ _ _ (Just (Etc.Validated scheme _)) _) = scheme






