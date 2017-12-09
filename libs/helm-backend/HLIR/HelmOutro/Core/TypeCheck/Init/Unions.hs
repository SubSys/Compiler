{-# LANGUAGE NoImplicitPrelude #-}
module HLIR.HelmOutro.Core.TypeCheck.Init.Unions where


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
-- ~ HelmOutro IR
import qualified HLIR.HelmOutro.Data.Payload as Payload

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
import qualified HLIR.HelmOutro.Core.TypeCheck.Data.Env             as Env
import qualified HLIR.HelmOutro.Core.TypeCheck.Data.Canonical.Ident as CID
-- *


{-# ANN module ("HLint: ignore" :: String) #-}







genUnionSigs :: Decl.Union -> [(CID.Ident, T.Scheme)]
genUnionSigs union@(Decl.Union _ _ cs) =
    map (genConstrBinderSig union) cs



genConstrBinderSig :: Decl.Union -> Decl.Constructor -> (CID.Ident, T.Scheme)
genConstrBinderSig decl@(Decl.Union unionName as _) (Decl.Constructor conName args) =
    let t = Fold.foldr T.Arr (union2Type decl) args
        
        -- Resulting Data
        ident = CID.ident conName
        scheme = T.Forall as t
    in
        (ident, scheme)


union2Type :: Decl.Union -> T.Type
union2Type (Decl.Union unionName as _) =
    T.Union unionName (map toTyVar as)

    where
        toTyVar :: ID.Low -> T.Type
        toTyVar (ID.Low txt _) = T.Var (ID.Low txt Nothing)



