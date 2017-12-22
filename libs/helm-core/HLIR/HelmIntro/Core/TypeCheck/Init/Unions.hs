{-# LANGUAGE NoImplicitPrelude #-}
module HLIR.HelmIntro.Core.TypeCheck.Init.Unions (
    genUnionSigs
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
import qualified HLIR.HelmIntro.AST.Data.Base.Etc    as Etc
import qualified HLIR.HelmIntro.AST.Data.Base.Ident  as ID
import qualified HLIR.HelmIntro.AST.Data.Base.Types  as T
import qualified HLIR.HelmIntro.AST.Data.Base.Values as V

-- ~~ TermLevel
import qualified HLIR.HelmIntro.AST.Data.TermLevel.Expressions as E
import qualified HLIR.HelmIntro.AST.Data.TermLevel.Patterns    as P

-- ~~ TopLevel
import qualified HLIR.HelmIntro.AST.Data.TopLevel.Fixities  as Decl
import qualified HLIR.HelmIntro.AST.Data.TopLevel.Functions as Decl
import qualified HLIR.HelmIntro.AST.Data.TopLevel.Unions    as Decl


--- Local
import qualified HLIR.HelmIntro.Core.TypeCheck.Data.Interface.TypesEnv as TI
import qualified HLIR.HelmIntro.AST.Auxiliary.Canonical.Ident as CID
-- *


{-# ANN module ("HLint: ignore" :: String) #-}







genUnionSigs :: Decl.Union -> [(CID.Ident, T.Scheme)]
genUnionSigs union@(Decl.Union _ _ cs _) =
    map (genConstrBinderSig union) cs



genConstrBinderSig :: Decl.Union -> Decl.Constructor -> (CID.Ident, T.Scheme)
genConstrBinderSig decl@(Decl.Union unionName as _ _) (Decl.Constructor conName args _) =
    let t = Fold.foldr T.Arr' (union2Type decl) args
        
        -- Resulting Data
        ident = CID.ident conName
        scheme = T.Forall as t
    in
        (ident, scheme)


union2Type :: Decl.Union -> T.Type
union2Type (Decl.Union unionName as _ _) =
    T.Union' unionName (map toTyVar as)

    where
        toTyVar :: ID.Low -> T.Type
        toTyVar (ID.Low txt _ _) = T.Var' (ID.Low' txt Nothing)



