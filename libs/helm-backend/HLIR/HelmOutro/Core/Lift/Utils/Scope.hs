{-# LANGUAGE NoImplicitPrelude #-}
module HLIR.HelmOutro.Core.Lift.Utils.Scope (
      freeVars
    , without
    , isClosed
    , withLocalLTEnv
    , freshVar
    , freshIdent
) where


-- *
import Core
import Core.Control.Flow ((|>), (<|))
import Prelude (return, String, IO, show, error, (<$>))

import Data.List.Index  (imap)

import Data.Data (Data, Typeable)

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

-- ~ Auxiliary Nodes
import qualified HLIR.HelmOutro.AST.Auxiliary.Canonical.Ident as CID
import qualified HLIR.HelmOutro.Core.Lift.Data.System as Sys
-- *




freeVars :: (Data a, Typeable a) => a -> [CID.Ident]
freeVars x =
    let result = refs `without` binders
    in 
        Set.toList $ Set.fromList result

    where
        refs    = CID.idents [x | x@ID.Ref{}    <- Uni.universeBi x]
        binders = CID.idents [x | x@ID.Binder{} <- Uni.universeBi x]


without :: Eq a => [a] -> [a] -> [a]
without =
    Fold.foldr (List.filter . (/=))


-- | 
isClosed :: (Data a, Typeable a) =>  a -> Bool
isClosed x =
    List.length (freeVars x) == 0







-- *
-- | Lift (State) Related Utils
-- *


withLocalLTEnv :: Sys.LiftedEnv -> Sys.Lift a -> Sys.Lift a
withLocalLTEnv ls1 =
    
    M.local modEnv

    where
        modEnv (ls2, gs) =
            (Map.union ls1 ls2, gs)


freshVar :: Sys.Lift E.Expr
freshVar =
    (E.Var . CID.toRef) <$> freshIdent


freshIdent :: Sys.Lift CID.Ident
freshIdent = do
    (Sys.Counter i) <- M.get
    
    M.put (Sys.Counter $ i + 1)
    
    return $ CID.Ident $ pack i
    
    where
        pack :: Int -> Text
        pack = Text.pack . show



