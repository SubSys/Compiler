{-# LANGUAGE NoImplicitPrelude #-}
module SLIR.HelmSyntax.Core.Program.SDD.Driver (
      runSDD
    , runSDD'
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

-- ~~ Auxiliary AST - Nodes & Utils
import qualified SLIR.HelmSyntax.AST.Auxiliary.Canonical.Ident            as CID
import qualified SLIR.HelmSyntax.AST.Toolbox.SudoFFI                      as SudoFFI
import qualified SLIR.HelmSyntax.AST.Toolbox.TopLevel.Functions.Recursive as Rec


--- Local Prelude
import SLIR.HelmSyntax.Core.Program.SDD.Inference.Utils.Syntax.Base (enter, binder)


--- Local
import qualified SLIR.HelmSyntax.Core.Program.SDD.Inference.Data.Env               as Env
import qualified SLIR.HelmSyntax.Core.Program.SDD.Data.Report                      as Report
import qualified SLIR.HelmSyntax.Core.Program.SDD.Inference.Data.System            as Sys
import qualified SLIR.HelmSyntax.Core.Program.SDD.Inference.Utils.Syntax.Scope     as Scope
import qualified SLIR.HelmSyntax.Core.Program.SDD.Inference.Utils.TypeSystem       as TS
import qualified SLIR.HelmSyntax.Core.Program.SDD.Solver.Data.Constraint           as Con
import qualified SLIR.HelmSyntax.Core.Program.SDD.Inference.Utils.Auxiliary        as Aux
import qualified SLIR.HelmSyntax.Core.Program.SDD.Solver.Engine                    as Solver
import qualified SLIR.HelmSyntax.Core.Program.SDD.Subst.Expr           as ExSub
import qualified SLIR.HelmSyntax.Core.Program.SDD.Subst.Types          as TySub
import qualified SLIR.HelmSyntax.Core.Program.SDD.Syntax.TopLevel.Functions        as Decl
import qualified SLIR.HelmSyntax.Core.Program.SDD.Resolve.Engine                   as Resolve

-- ~ Inits
import qualified SLIR.HelmSyntax.Core.Program.SDD.Init.Decls  as Init
import qualified SLIR.HelmSyntax.Core.Program.SDD.Init.Unions as Init

-- Post-Work
import qualified SLIR.HelmSyntax.Core.Program.SDD.PostWork.Lift as Lift
import qualified SLIR.HelmSyntax.Core.Program.SDD.PostWork.Desugar as Desugar
-- *







runSDD :: IO (Either Text Payload.Program) -> IO (Either Text Payload.Program)
runSDD upstream = do
    result <- upstream
    
    case result of
        Left err -> return $ Left err
        Right payload ->
            return $ runSDD' payload




runSDD' :: Payload.Program -> Either Text Payload.Program
runSDD' payload =
    let 
        -- Setups
        fns = Payload.getFunctions payload
        uns = Payload.getUnions payload
        
        -- Initial Data
        typesEnv = initialEnv uns

        -- Finish
        result = Resolve.resolveDecls Decl.inferDecl typesEnv fns
        
    in
        case result of
            Left err ->
                Left $ formatError err

            Right (fns', _, _) ->
                Right $ Payload.updateFunctions (finalize fns') payload




finalize :: [Decl.Function] -> [Decl.Function]
finalize decls =
    decls |> Lift.processDecls
          |> Desugar.desugar


formatError :: Report.TypeError -> Text
formatError  err =
    Text.pack $ PP.prettyShow err




-- *
-- | Internal
-- *


initialEnv :: [Decl.Union] -> Env.ExprEnv
initialEnv us =
    map Init.genUnionSigs us
            |> flatten
            |> map toEntry
            |> Map.unions

    where
        toEntry (name, scheme) = Map.singleton name (Env.Normal scheme)







