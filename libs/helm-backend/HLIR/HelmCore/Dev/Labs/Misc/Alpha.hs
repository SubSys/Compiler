{-# LANGUAGE NoImplicitPrelude #-}
module HLIR.HelmCore.Dev.Labs.Misc.Alpha where


-- *
import Core
import Core.Control.Flow ((|>), (<|))
import Prelude (return, String, IO, show, error)

import Data.List.Index  (imap)

import qualified Numeric.Natural as Natural

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


--- Dev
import qualified Dev.Samples.Basic      as BasicSamples
import qualified Dev.Samples.Complex    as ComplexSamples
import qualified Dev.Samples.TestParser as ParserSample

import qualified Text.Show.Prettyprint as PP

import qualified HLIR.HelmCore.Render.Utils as Display


-- ~ Upstream
import qualified SLIR.HelmSyntax.Core as HelmSyntax

-- ~ HelmSyntax IR
import qualified HLIR.HelmCore.Data.Payload as Payload

--- Local Deps
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
import qualified HLIR.HelmCore.Core.Indexing.Driver as Driver
-- *


{-# ANN module "HLint: ignore" #-}





sample =
    ParserSample.sampleOne
        |> HelmSyntax.frontend
        |> HelmSyntax.toHelmCore
        |> Driver.globalize

run = do
    input <- sample
    
    case input of
        Left err      -> putStrLn (Text.unpack err)
        Right payload -> run' payload



run' payload =
    -- M.mapM_ PP.prettyPrint functions
    -- putStrLn $ Text.unpack $ Display.renderFunctions functions
    
    -- arguments $ List.head functions
    M.mapM_ arguments functions
    
    where
        functions  = Payload.getFunctions payload


-- arguments :: Decl.Function -> Decl.Function
arguments :: Decl.Function -> IO ()
arguments (Decl.Function name expr (T.Forall as t)) =
    let (args, expr') = splitArgs expr
        ts            = splitTypes t
    in do
        -- PP.prettyPrint (format ts)
        PP.prettyPrint (partialArguments args ts expr')
    
    where
        splitTypes :: T.Type -> [T.Type]
        splitTypes (T.Arr t1 t2) =
            t1 : splitTypes t2
        splitTypes x = [x]
        
        splitArgs :: E.Expr -> ([ID.Binder], E.Expr)
        splitArgs (E.Abs b e) =
            let 
                (bs, e') = splitArgs e
            in
                (b : bs, e')
        
        splitArgs x = ([], x)




partialArguments :: [ID.Binder] -> [T.Type] -> E.Expr -> Maybe E.Expr
partialArguments args ts expr
    | (tsLength >= 2) =
        if List.length inputTs == argsLength then
            Nothing
        else
            let
                numOfExtraArgs = List.length inputTs
            in
                Just $ applyPartialArguments expr (genArgs numOfExtraArgs)

    | tsLength == 1 =
        Nothing

    where
        argsLength = List.length args
        tsLength   = List.length ts
        
        (inputTs, outputType) = splitTypes ts
        
        
        splitTypes :: [T.Type] -> ([T.Type], T.Type)
        splitTypes ts =
            let ins = List.init ts
                out = List.last ts
            in
                (ins, out)

genArgs :: Int -> [ID.Binder]
genArgs 0 = []
genArgs i
    | i < 0 =
        error $ "Int should be ‘non-negative’ number!"
    | otherwise =
        newArg (show i) : genArgs (i - 1)
    
    where
        newArg idx =
            let
                prefix = Text.pack "!"
                name   = Text.pack idx
            in
                ID.Binder (prefix `Text.append` name) Nothing


applyPartialArguments :: E.Expr -> [ID.Binder] -> E.Expr
applyPartialArguments e bs =
    let e' = List.foldl E.App e vars
    in
        List.foldr E.Abs e' bs

    where
        vars = map E.Var $ map binder2Ref bs
        
        binder2Ref :: ID.Binder -> ID.Ref
        binder2Ref (ID.Binder txt ns) =
            ID.Ref txt ns






format x =
    Uni.transformBi f x
    where
        f :: ID.Binder -> ID.Binder
        f (ID.Binder txt ns) =
            let 
                x = Text.pack "º"
                y = Text.pack "G"
                txt' = Text.replace x y txt
            in
                ID.Binder txt' ns

