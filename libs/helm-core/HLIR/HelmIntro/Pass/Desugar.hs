{-# LANGUAGE NoImplicitPrelude #-}
module HLIR.HelmIntro.Pass.Desugar (
      desugarBinOps
    , desugarIfTerms
    , desugarFnArgs
) where


-- *
import Core
import Core.Control.Flow ((|>), (<|))
import Prelude (return, String, IO, show, error)

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


--- Dev
import qualified Dev.Samples.Basic      as BasicSamples
import qualified Dev.Samples.Complex    as ComplexSamples
import qualified Dev.Samples.TestParser as ParserSample

import qualified Text.Show.Prettyprint as PP
import qualified HLIR.HelmIntro.Render.Utils as Display

-- ~ HelmSyntax Cores
import qualified HLIR.HelmIntro.Core.Module.Parser.Driver    as Driver
import qualified HLIR.HelmIntro.Core.TypeCheck.Driver as Driver

-- ~ HelmSyntax IR
import qualified HLIR.HelmIntro.Data.Interface.Module.Payload as Payload

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
import qualified HLIR.HelmIntro.Pass.Desugar.Helpers as Helper
-- *







desugarFnArgs :: [Decl.Function] -> [Decl.Function]
desugarFnArgs =
    map desugarFnArg 
    where
        desugarFnArg :: Decl.Function -> Decl.Function
        desugarFnArg = Uni.transform fn
        
        fn :: Decl.Function -> Decl.Function
        fn (Decl.FnDecl name args expr sig meta) =
            let expr' = go args expr
            in
                Decl.FnDecl name [] expr' sig meta

        fn (Decl.OpDecl name args expr sig meta) =
            let expr' = go args expr
            in
                Decl.OpDecl name [] expr' sig meta



        go :: [ID.Low] -> E.Expr -> E.Expr
        go args expr =
            Fold.foldr E.Abs' expr args


desugarBinOps :: [Decl.Function] -> [Decl.Function]
desugarBinOps fns =
    fns |> deSugarBinders
        |> deSugarRefs
    
    where
        symBinders = [txt | (ID.Sym txt _ _) <- Uni.universeBi fns]
        
        pack i txt =
            let idx = Text.pack $ show i
                prefix = Text.pack "!op"
            in
                (txt, prefix `Text.append` idx)
        
        syms = Map.fromList $ imap pack symBinders
        
        extract :: ID.Sym -> Text
        extract (ID.Sym txt _ _) = txt
        
        
        
        deSugarBinders :: [Decl.Function] -> [Decl.Function]
        deSugarBinders = Uni.transformBi deSugarBinders'
        
        deSugarRefs :: [Decl.Function] -> [Decl.Function]
        deSugarRefs = Uni.transformBi deSugarRefs'
        
        
        
        
        deSugarBinders' :: Decl.Function -> Decl.Function
        deSugarBinders' (Decl.OpDecl sym args expr sig meta) =
            case Map.lookup (extract sym) syms of
                Nothing -> error "Internal Compiler Error: 'HelmSyntax - deSugarBinders' failure!"
                Just n  ->
                    Decl.FnDecl (ID.Low n Nothing Nothing) args expr sig meta
        
        deSugarBinders' x = x
        
        
        deSugarRefs' :: E.Expr -> E.Expr
        deSugarRefs' (E.BinOp sym e1 e2 _) =
            case Map.lookup (extract sym) syms of
                Nothing -> error "Internal Compiler Error: 'HelmSyntax - deSugarRefs' failure!"
                Just n ->
                    let n' = E.Var' (ID.Low' n Nothing)
                    in
                        E.App' (E.App' n' e1) e2
                        

        
        deSugarRefs' x = x





desugarIfTerms :: [Decl.Function] -> [Decl.Function]
desugarIfTerms =
    Uni.transformBi go
    where
        go :: E.Expr -> E.Expr
        go = Helper.desugarIfTerms





