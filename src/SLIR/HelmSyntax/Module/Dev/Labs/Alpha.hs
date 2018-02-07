{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns #-}
module SLIR.HelmSyntax.Module.Dev.Labs.Alpha where


-- *
import Core
import Core.Control.Flow ((|>), (<|))
import Core.List.Util    (flatten, singleton)
import Data.Monoid ((<>))
import Prelude
    (return
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

-- + OS APIS & Related
import qualified System.IO as SIO

-- + Megaparsec & Related
import qualified Text.Megaparsec.Char       as C
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Megaparsec            as MP

-- + Frameworks
import Framework.Text.Parser

-- + Dev & Debugging
import qualified Text.Show.Prettyprint as PP

-- + HelmSyntax Module Interface
import qualified SLIR.HelmSyntax.Module.Data.Interface as I

-- + HelmSyntax AST Renderer
import qualified SLIR.HelmSyntax.AST.Render.Syntax.Driver as Syntax

-- + HelmSyntax AST Utils
import qualified SLIR.HelmSyntax.AST.Utils.Scope           as Scope
import qualified SLIR.HelmSyntax.AST.Utils.Auxiliary.Ident as ID

-- + HelmSyntax AST
-- ++ Base
import qualified SLIR.HelmSyntax.AST.Data.Semantic.Base.Etc      as Etc
import qualified SLIR.HelmSyntax.AST.Data.Semantic.Base.Ident    as ID
import qualified SLIR.HelmSyntax.AST.Data.Semantic.Base.Types    as T
import qualified SLIR.HelmSyntax.AST.Data.Semantic.Base.Values   as V
import qualified SLIR.HelmSyntax.AST.Data.Semantic.Base.Metadata as Meta
import qualified SLIR.HelmSyntax.AST.Data.Semantic.Base.Header   as Header

-- ++ TermLevel
import qualified SLIR.HelmSyntax.AST.Data.Semantic.TermLevel.Expr     as E
import qualified SLIR.HelmSyntax.AST.Data.Semantic.TermLevel.Patterns as P

-- ++ TopLevel
import qualified SLIR.HelmSyntax.AST.Data.Semantic.TopLevel.Fixities  as Decl
import qualified SLIR.HelmSyntax.AST.Data.Semantic.TopLevel.Functions as Decl
import qualified SLIR.HelmSyntax.AST.Data.Semantic.TopLevel.Unions    as Decl

-- + Local
import qualified SLIR.HelmSyntax.Module.Core.Parser.Driver as Driver

-- + Tmp...
import qualified SLIR.HelmSyntax.Internal.AST as IR
-- *



{-# ANN module ("HLint: ignore" :: String) #-}





inputFilePath = "/Users/colbyn/SubSystems/Compiler/etc/resources/samples/test-parser/One.helm"





upstream =
    let filePath   = inputFilePath
        sourceCode = SIO.readFile inputFilePath
    in
        sourceCode
            |> Driver.runModuleParser filePath



run = do
    result <- upstream
    case result of
        Left err ->
            putStrLn $ Text.unpack err
        Right payload ->
            run' payload



run' payload = do
    
    (TIO.putStrLn . Syntax.renderFunctions) fns
    
    putStrLn $ List.replicate 90 '-'
    putStrLn "\n"
    
    
    M.mapM_ PP.prettyPrint $ map freeVars fns
    
    where
        fns = I.getFunctions payload
        uns = I.getUnions payload






without :: Eq a => [a] -> [a] -> [a]
without =
    Fold.foldr (List.filter . (/=))



freeVars :: IR.Function -> [ID.Ident]
freeVars = (F.cata algebra) . IR.generalizeFunction
    where
        algebra :: IR.ExprF [ID.Ident] -> [ID.Ident]
        algebra (IR.VarExprF x m)      = [x]
        algebra (IR.AbsExprF (Etc.Binder_ ident) expr m)   =
            expr `without` [ident]
        
        algebra (IR.LetExprF fns expr meta) =
            expr

        algebra (IR.FunctionF name args expr sig meta) =
            expr `without` (ID.gets $ name : args)
        
        
        algebra x = Fold.fold x


-- freeVars' :: IR.Function -> [ID.Ident]
freeVars' = (F.para algebra) . IR.generalizeFunction
    where
        -- algebra :: IR.ExprF [ID.Ident] -> [ID.Ident]
        -- algebra (IR.VarExprF x m)      = [x]
        algebra (IR.VarExprF x m)      = [x]
        
        -- algebra (IR.AbsExprF (Etc.Binder_ ident) expr m)   =
        --     expr `without` [ident]
        
        algebra (IR.AbsExprF arg (f, vs) m)   = vs
        

        -- algebra (IR.LetExprF fns expr meta) =
        --     expr
        -- 
        -- algebra (IR.FunctionF name args expr sig meta) =
        --     expr `without` (ID.gets $ name : args)
        
        -- algebra x = Fold.fold x








