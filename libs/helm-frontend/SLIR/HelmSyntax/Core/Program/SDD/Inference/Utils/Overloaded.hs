{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
module SLIR.HelmSyntax.Core.Program.SDD.Inference.Utils.Overloaded (
      genOverloaded
    -- , isOverloaded
) where


-- *
import Core
import Core.Control.Flow ((|>), (<|))
import Core.List.Util    (flatten)
import Data.Data (Data, Typeable)

import Prelude (return, String, IO, show, error, (<$>), (>>), (>>=), (>>), lookup)

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


--- Local Internal-Utils
import qualified SLIR.HelmSyntax.Core.Program.SDD.Inference.Utils.Internal.FreshIdents as Fresh

--- Local
import qualified SLIR.HelmSyntax.Core.Program.SDD.Inference.Data.Env               as Env
import qualified SLIR.HelmSyntax.Core.Program.SDD.Solver.Data.Constraint           as Con
import qualified SLIR.HelmSyntax.Core.Program.SDD.Data.Report                      as Report
import qualified SLIR.HelmSyntax.Core.Program.SDD.Inference.Data.System            as Sys
import qualified SLIR.HelmSyntax.Core.Program.SDD.Subst.Types                      as TySub
import qualified SLIR.HelmSyntax.Core.Program.SDD.Inference.Utils.TypeSystem       as TS
-- *




genOverloaded :: [T.Type] -> Sys.Infer T.Type
genOverloaded ts
    | List.all (== base1) restBases = do
        t <- TS.instantiate $ TS.closeOver base1
        -- *

        -- *
        superTv <- TS.freshType
        let t2 = T.Superposed superTv ts
        -- *
        
        -- *
        M.tell [Con.TypeCon t t2]
        -- *
        
        -- *
        return t
    
    | otherwise =
        M.throwError (Report.ConflictingOverloadedTypeArity ts)


    where
        
        xs1@(base1:restBases) = map base ts
        
        base :: T.Type -> T.Type
        base x = fst $ M.runState (Uni.transformM f x) [0..]
        
        f :: (M.MonadState [Int] m) => T.Type -> m T.Type
        f (T.Arr t1 t2 _) = return $ T.Arr' t1 t2
        
        f x = do
            (x:xs) <- M.get
            M.put xs
            
            return $ T.Var' (ID.Low' (label x) Nothing)
        
        
        label x =
            Text.pack $ show x
        
        
        -- checkBases :: T.Type -> T.Type -> Bool
        -- checkBases 








