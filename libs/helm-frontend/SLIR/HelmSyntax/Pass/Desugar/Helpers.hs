{-# LANGUAGE NoImplicitPrelude #-}
module SLIR.HelmSyntax.Pass.Desugar.Helpers where


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
import qualified SLIR.HelmSyntax.Render.Utils as Display

-- ~ HelmSyntax Cores
import qualified SLIR.HelmSyntax.Core.Parser.Driver    as Driver
import qualified SLIR.HelmSyntax.Core.TypeCheck.Driver as Driver

-- ~ HelmSyntax IR
import qualified SLIR.HelmSyntax.Data.Payload as Payload

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

-- ~~ ...
import qualified SLIR.HelmSyntax.AST.Data.Base.Ident.Advance as SudoID
-- *




desugarIfTerms :: E.Expr -> E.Expr
desugarIfTerms expr =
    let expr' = Uni.transform init expr
    in
        Uni.transform toCase expr'

    where
        packIf (con, body) def = E.If' [(con, body)] def
        init (E.If intros elseExpr meta) =
            Fold.foldr packIf elseExpr intros
        init x = x
        
        
        toCase (E.If [(con, trueExpr)] falseExpr m) =
            E.Case' con
                [ trueBranch trueExpr
                , falseBranch falseExpr
                ]
        toCase x = x
        
        trueBranch :: E.Expr -> P.CaseAlt
        trueBranch e = P.CaseAlt (P.Lit' (V.Bool' True)) e Nothing
        
        falseBranch :: E.Expr -> P.CaseAlt
        falseBranch e = P.CaseAlt (P.Lit' (V.Bool' False)) e Nothing



