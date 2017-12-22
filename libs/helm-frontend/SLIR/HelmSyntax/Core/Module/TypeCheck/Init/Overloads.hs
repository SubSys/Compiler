{-# LANGUAGE NoImplicitPrelude #-}
module SLIR.HelmSyntax.Core.Module.TypeCheck.Init.Overloads (
    initOverloads
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


--- Local Deps
-- ~ HelmSyntax AST
-- ~~ Base
import qualified SLIR.HelmSyntax.AST.Data.Base.Etc    as Etc
import qualified SLIR.HelmSyntax.AST.Data.Base.Ident  as ID
import qualified SLIR.HelmSyntax.AST.Data.Base.Types  as T
import qualified SLIR.HelmSyntax.AST.Data.Base.Values as V

-- ~~ TermLevel
import qualified SLIR.HelmSyntax.AST.Data.TermLevel.Expressions as E
import qualified SLIR.HelmSyntax.AST.Data.TermLevel.Patterns    as P

-- ~~ TopLevel
import qualified SLIR.HelmSyntax.AST.Data.TopLevel.Fixities  as Decl
import qualified SLIR.HelmSyntax.AST.Data.TopLevel.Functions as Decl
import qualified SLIR.HelmSyntax.AST.Data.TopLevel.Unions    as Decl


--- Local
import SLIR.HelmSyntax.Core.Module.TypeCheck.Data.System (Overloaded)

import qualified SLIR.HelmSyntax.AST.Auxiliary.Canonical.Ident             as CID
import qualified SLIR.HelmSyntax.AST.Toolbox.TopLevel.Functions.Overloaded as Overloaded
import qualified SLIR.HelmSyntax.Core.Module.TypeCheck.Data.TypeSystem            as TS
import qualified SLIR.HelmSyntax.Core.Module.TypeCheck.Data.Interface.TypesEnv             as TI
-- *


{-# ANN module ("HLint: ignore" :: String) #-}




initOverloads :: [Decl.Function] -> Overloaded
initOverloads decls =
    let decls' = Overloaded.group decls
            |> List.filter (\x -> List.length x >= 2)
        
        idents = map extractName decls'
        schems = map extractSchems decls'
        
    in
        Map.fromList $ List.zip idents schems
    
    where
        extractName :: [Decl.Function] -> CID.Ident
        extractName fns =
            let
                -- NOTE: every ID should be the same...
                (x:xs) = map CID.ident fns

            in
                x
        
        extractSchems :: [Decl.Function] -> [T.Scheme]
        extractSchems = map extractSchem
        
        extractSchem :: Decl.Function -> T.Scheme
        
        extractSchem (Decl.FnDecl _ _ _ (Just (Etc.Validated ty _)) _) =
            TS.normalizeScheme ty

        extractSchem (Decl.OpDecl _ _ _ (Just (Etc.Validated ty _)) _) =
            TS.normalizeScheme ty

        extractSchem (Decl.FnDecl _ _ _ (Just (Etc.Unresolved ty _)) _) =
            TS.generalize TI.empty ty
        
        extractSchem (Decl.OpDecl _ _ _ (Just (Etc.Unresolved ty _)) _) =
            TS.generalize TI.empty ty
        



