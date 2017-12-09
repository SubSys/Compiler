{-# LANGUAGE NoImplicitPrelude #-}
module HLIR.HelmCore.Core.Lift.Dev where


-- *
import Core
import Core.Control.Flow ((|>), (<|))
import Core.List.Util (flatten)
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


--- Dev
import qualified Dev.Samples.Basic      as BasicSamples
import qualified Dev.Samples.Complex    as ComplexSamples
import qualified Dev.Samples.TestParser as ParserSample
import qualified HLIR.HelmCore.Render.Utils as Display

-- ~ Upstream
import qualified SLIR.HelmSyntax.Core as HelmSyntax



--- Local Deps
-- ~ HelmCore Drivers
import qualified HLIR.HelmCore.Core.Indexing.Driver  as Driver
import qualified HLIR.HelmCore.Core.TypeCheck.Driver as Driver

-- ~ HelmCore IR
import qualified HLIR.HelmCore.Data.Payload as Payload

-- ~ HelmCore AST
-- ~~ Base
import qualified HLIR.HelmCore.AST.Base.Ident  as ID
import qualified HLIR.HelmCore.AST.Base.Types  as T
import qualified HLIR.HelmCore.AST.Base.Values as V
-- ~~ TermLevel
import qualified HLIR.HelmCore.AST.TermLevel.Expressions as E
import qualified HLIR.HelmCore.AST.TermLevel.Patterns    as P
-- ~~ TopLevel
import qualified HLIR.HelmCore.AST.TopLevel.Functions as Decl
import qualified HLIR.HelmCore.AST.TopLevel.Unions    as Decl


--- Local

-- ~ Lift Utils
import qualified HLIR.HelmCore.Core.Lift.Auxiliary.Utils as Util
import qualified HLIR.HelmCore.Core.Lift.Data.Canonical.Ident as CID
-- *



{-# ANN module "HLint: ignore" #-}




upstream =
    ParserSample.sampleOne
        |> HelmSyntax.frontend
        |> HelmSyntax.toHelmCore
        |> Driver.typeCheck
        |> Driver.stdIndexer



run = do
    input <- upstream
    
    case input of
        Left err      -> putStrLn (Text.unpack err)
        Right payload -> run' payload



run' payload =
    
    putStrLn $ Text.unpack $ Display.renderFunctions $ flatten (map alpha fns)
    
    -- case res of
    --     Left err -> PP.prettyPrint err
    --     Right decls ->
    --         putStrLn $ Text.unpack $ Display.renderFunctions decls

    where
        -- res = typeCheck payload xs
        -- xs = map alpha fns
        
        fns = Payload.getFunctions payload



-- *
-- | Misc. Tmp...
-- *

format x =
    Uni.transformBi f x

    where
        
        g = Text.pack "º"
        l = Text.pack "ª"
        
        g' = Text.pack "G"
        l' = Text.pack "L"
        
        f :: Text -> Text
        f =
            Text.replace l l' . Text.replace g g'






alpha :: Decl.Function -> [Decl.Function]
alpha fn@(Decl.Function name@(ID.Binder txt _) expr scheme) =
    -- let expr' = Util.closConv (map CID.toBinder $ Util.freeVars expr) expr
    -- in
    --     Decl.Function name expr' scheme
        
        Util.eliminateLams (map CID.toBinder $ Util.freeVars expr) fn
        





typeCheck :: Payload.Module -> [Decl.Function] -> Either Text [Decl.Function]
typeCheck payload fns =
    let payload' = Payload.updateFunctions payload fns
    in
        case Driver.typeCheck' payload' of
            Left err -> Left err
            Right payload_ ->
                Right $ Payload.getFunctions payload_







