{-# LANGUAGE NoImplicitPrelude #-}
module HLIR.HelmOutro.Core.PreLift.Driver (
      prelift
    , prelift'
) where


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
-- ~ HelmOutro Payload
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
import qualified HLIR.HelmOutro.Core.PreLift.Pass.Init.Uncurry      as UncurryPass
import qualified HLIR.HelmOutro.Core.PreLift.Pass.Mid.InsertImpArgs as ImpArgsPass
import qualified HLIR.HelmOutro.Core.PreLift.Pass.End.HoistArgs     as HoistArgsPass
import qualified HLIR.HelmOutro.Core.PreLift.Utils.InitEnv          as Util
-- *


prelift :: IO (Either Text Payload.Module) -> IO (Either Text Payload.Module)
prelift upstream = do
    result <- upstream
    
    case result of
        Left err -> return $ Left err
        Right payload ->
            return $ prelift' payload


prelift' :: Payload.Module -> Either Text Payload.Module
prelift' payload =
    let decls = pipeline payload
    in
        Right $ Payload.updateFunctions payload decls


-- *
-- | Internal
-- *

pipeline :: Payload.Module -> [Decl.Function]
pipeline payload =
    let fns  = Payload.getFunctions payload
        env  = Util.initEnv fns
    in
        UncurryPass.uncurryFuns fns
            |> ImpArgsPass.insertImpArgs env
            |> HoistArgsPass.hoistArgs




