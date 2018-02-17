{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
module HLIR.HelmFlat.Core.TypeCheck.Inference.Syntax.Constrain (
    unify
    
    -- Unify Etc.
  , app
  , binOpApp
  , unifySignature
  , call
  , overloaded
) where


-- *
import Core
import Core.Control.Flow ((|>), (<|))
import Core.List.Util    (flatten, singleton)
import Data.Monoid ((<>))
import Prelude
    ( return
    , String
    , IO
    , show
    , error
    , (<$>)
    , (>>=)
    , (>>)
    , fromIntegral
    )

import qualified Prelude    as Pre
import qualified Core.Utils as Core

import qualified Control.Monad              as M
import qualified Control.Monad.State        as M
import qualified Control.Monad.Except       as M
import qualified Control.Monad.RWS          as M
import qualified Control.Monad.Identity     as M
import qualified Control.Monad.Reader       as M
import qualified Control.Monad.Writer       as M
import qualified Control.Monad.Trans        as M

import qualified Data.List                    as List
import qualified Data.Text                    as Text
import qualified Data.Text.IO                 as TIO
import qualified Data.Map                     as Map
import qualified Data.Set                     as Set
import qualified Data.Foldable                as Fold
import qualified Data.Monoid                  as Monoid
import qualified Data.Maybe                   as Maybe
import qualified Data.Either                  as Either
import qualified Data.Char                    as Char
import qualified Data.Word                    as Word
import qualified Data.STRef                   as ST
import qualified Data.Bits                    as Bit
import qualified Data.Fixed                   as Fixed
import qualified Data.Vector.Unboxed          as V
import qualified Data.Vector.Unboxed.Mutable  as MV
import qualified Data.Vector.Generic          as VG
import qualified Data.IORef                   as IORef
import qualified Data.ByteString              as BS
import qualified Data.Functor                 as Fun


-- + Recursion Schemes & Related
import qualified Data.Functor.Foldable       as F
import qualified Data.Generics.Uniplate.Data as Uni


-- + Dev & Debugging
import qualified Text.Show.Prettyprint as PP

-- + HelmFlat Module Interface
import qualified HLIR.HelmFlat.Module.Data.Interface as I

-- + HelmFlat AST Utils
import qualified HLIR.HelmFlat.AST.Utils.Scope           as Scope
import qualified HLIR.HelmFlat.AST.Utils.Auxiliary.Ident as ID
import qualified HLIR.HelmFlat.AST.Utils.Auxiliary.Type  as T

-- + HelmFlat AST
-- ++ Base
import qualified HLIR.HelmFlat.AST.Data.Semantic.Base.Etc      as Etc
import qualified HLIR.HelmFlat.AST.Data.Semantic.Base.Ident    as ID
import qualified HLIR.HelmFlat.AST.Data.Semantic.Base.Types    as T
import qualified HLIR.HelmFlat.AST.Data.Semantic.Base.Values   as V
import qualified HLIR.HelmFlat.AST.Data.Semantic.Base.Metadata as Meta

-- ++ TermLevel
import qualified HLIR.HelmFlat.AST.Data.Semantic.TermLevel.Expr     as E
import qualified HLIR.HelmFlat.AST.Data.Semantic.TermLevel.Patterns as P

-- ++ TopLevel
import qualified HLIR.HelmFlat.AST.Data.Semantic.TopLevel.Fixities  as Decl
import qualified HLIR.HelmFlat.AST.Data.Semantic.TopLevel.Functions as Decl
import qualified HLIR.HelmFlat.AST.Data.Semantic.TopLevel.Unions    as Decl

-- + Local
import qualified HLIR.HelmFlat.Core.TypeCheck.Inference.Data.System      as Sys
import qualified HLIR.HelmFlat.Core.TypeCheck.Solver.Data.Constraint     as Con
import qualified HLIR.HelmFlat.Core.TypeCheck.Data.Report                as Report
import qualified HLIR.HelmFlat.Core.TypeCheck.Inference.Data.Env         as Env
import qualified HLIR.HelmFlat.Core.TypeCheck.Inference.Utils.TypeSystem as TS
import qualified HLIR.HelmFlat.Core.TypeCheck.Inference.Syntax.Scope     as Scope

-- ++ Local Helpers
import qualified HLIR.HelmFlat.Core.TypeCheck.Inference.Syntax.Constrain.Overloaded as OL
-- *





{-# ANN module ("HLint: ignore" :: String) #-}





-- | Base unifier
-- Simply generate a constraint on the two provided types.
--
unify :: T.Type -> T.Type -> Sys.Infer ()
unify t1 t2 = M.tell [(t1, t2)]




-- | Unify 'Type' & 'Function Signature'
-- If there exists a type signature, unify such with the provided type
--
unifySignature :: T.Type -> Decl.Signature -> Sys.Infer ()

unifySignature t (Decl.Unresolved t' meta) =
    unify t t'

unifySignature t (Decl.Validated s meta) = do
    t' <- TS.instantiate s
    unify t t'

unifySignature t _ =
    M.tell []



app :: T.Type -> T.Type -> Sys.Infer T.Type
app t1 t2 = do
    tv <- TS.freshType
    
    unify t1 (t2 `T.Arr'` tv)
    
    return tv






binOpApp :: ID.Ident -> T.Type -> T.Type -> Sys.Infer T.Type
binOpApp sym t1 t2 = do
    res <- Scope.isOverloaded sym
    case res of
        Nothing -> binOpApp' sym t1 t2
        Just ss ->  do
            ts <- M.mapM TS.instantiate ss
            
            overloadedBinOp ts sym t1 t2



call :: ID.Ident -> T.Type -> [T.Type] -> Sys.Infer T.Type
call ident t ts = do
    isOverloaded <- Scope.isOverloaded ident
    -- *
    
    -- *
    tv <- TS.freshType
    -- *
    
    -- *
    let ts1 = Fold.foldr T.Arr' tv ts
    -- *
    
    -- *
    unify t ts1
    -- *
    
    -- *
    case isOverloaded of
        Nothing ->
            return tv
            
            
        Just ss -> do
            error
                $ Text.unpack
                $ Text.unlines
                    [ Text.pack "Currently handles this in the main expression inference check..."
                    , Text.pack "I.e. `HLIR.HelmFlat.Core.TypeCheck.Syntax.TermLevel.Expr`:"
                    , Text.pack "`inferExpr f (E.FunCall ident args ty meta)`"
                    ]





-- | Type check an overloaded function, returns a ‘superposed type’.
--
overloaded :: [T.Scheme] -> Sys.Infer T.Type
overloaded = OL.synthType





-- *
-- | Misc. Internal Helpers
-- *



-- | Default Constraints (not overloaded).
--


binOpApp' :: ID.Ident -> T.Type -> T.Type -> Sys.Infer T.Type
binOpApp' sym et1 et2 = do
    tv <- TS.freshType
    -- *

    -- *
    let t1 = et1 `T.Arr'` (et2 `T.Arr'` tv)
    t2 <- Scope.lookupEnv sym
    -- *

    -- *
    unify t1 t2
    -- *

    return tv


-- Should I use overloaded or superposed?

-- I.e. overloaded [t1 : ts]

overloadedBinOp :: [T.Type] -> ID.Ident -> T.Type -> T.Type -> Sys.Infer T.Type
overloadedBinOp ts sym et1 et2 = do
    tv <- TS.freshType
    
    (T.Var ident _) <- TS.freshType
    -- *

    -- *
    let t1 = et1 `T.Arr'` (et2 `T.Arr'` tv)
        t2 = T.Superposed [ident] ts
    -- *

    -- *
    unify t1 t2
    -- *
    
    
    -- *
    return tv










