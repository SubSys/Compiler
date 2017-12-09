{-# LANGUAGE NoImplicitPrelude #-}
module HLIR.HelmCore.Core.Lift.Auxiliary.Utils (
      freeVars
    , closConv
    , eliminateLams
) where


-- *
import Core
import Core.Control.Flow ((|>), (<|))
import Prelude (return, String, IO, show, error, (<$>), (>>=))

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
import qualified HLIR.HelmCore.Core.Lift.Data.Canonical.Ident as CID
import qualified HLIR.HelmCore.Core.Lift.Data.System          as Sys
-- *






freeVars :: E.Expr -> [CID.Ident]
freeVars expr =
    let result = refs `without` binders
    in 
        Set.toList $ Set.fromList result

    where
        refs    = CID.idents [x | x@ID.Ref{}    <- Uni.universeBi expr]
        binders = CID.idents [x | x@ID.Binder{} <- Uni.universeBi expr]
        
without :: Eq a => [a] -> [a] -> [a]
without =
    Fold.foldr (List.filter . (/=))


applyTo :: E.Expr -> [ID.Ref] -> E.Expr
-- applyTo e (a : as) = applyTo (E.App e $ E.Var a) as
-- applyTo e []       = e

applyTo = Fold.foldl (\ e a -> E.App e $ E.Var a)



-- closConv :: [Low] -> Expr -> Expr
-- closConv globals =
--     cata algebra
-- 
--     where
--         algebra :: ExprF Expr -> Expr
--         algebra (LamF vs e) =
--             let
--                 vars = freeVars' e `without'` (globals ++ vs)
--             in
--                 Lam (vars ++ vs) e `applyTo` vars
-- 
--         algebra e = F.embed e



closConv :: [ID.Binder] -> E.Expr -> E.Expr
closConv globals =
    Uni.transform f

    where
        f :: E.Expr -> E.Expr
        f (E.Abs b e) =
            let
                vars = freeVars e `without` CID.idents (globals ++ [b])
                binders = map CID.toBinder vars
                refs    = map CID.toRef vars
                
                lambda = Fold.foldr E.Abs e (binders ++ [b])
            in
                -- E.Abs (vars ++ b) e `applyTo` vars
                lambda `applyTo` refs
        
        f e = e


freshLow :: Sys.Lift CID.Ident
freshLow = do
    x <- M.get
    M.put (x + 1)

    return $ pack x
    
    where
        prefix = Text.pack "!"
        pack x = show x |> Text.pack
                        |> Text.append prefix
                        |> CID.Ident



-- liftLam :: Expr -> Lift Expr
-- liftLam =
--     cata algebra
--     where
--         -- algebra :: ExprF Expr -> Expr
--         algebra (AppF l r)    = App <$> l <*> r
--         algebra (VarF v)      = return $ Var v
--         algebra (PrimF p)     = return $ Prim p
--         algebra (LitF i)      = return $ Lit i
-- 
--         algebra (LamF vs e)  = do
--             name <- freshLow
-- 
--             Function name vs <$> e >>= M.tell . return
-- 
--             return $ Var name



liftLam :: E.Expr -> Sys.Lift E.Expr
liftLam (E.Abs b e) = do
    name <- freshLow
    
    let nameB = CID.toBinder name
    let nameR = CID.toRef name
    
    
    M.tell [Decl.Function nameB (E.Abs b e) scheme]
    
    
    return $ E.Var nameR
    
    where
        scheme = T.Forall [] $ T.Var (ID.Low (Text.pack "a") Nothing )

liftLam x = return x





eliminateLams :: [ID.Binder] -> Decl.Function -> [Decl.Function]
eliminateLams globals (Decl.Function name e scheme) =
    Decl.Function name e' scheme : defs

    where
        (e', defs) = Sys.runLift (Uni.transformM liftLam $ closConv globals e)





