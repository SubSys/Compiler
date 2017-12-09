{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module CGIR.Metal.AST.BlockLevel.Stmts (
      IR.Stmt
    , pattern Default
    , pattern Break
    , pattern Continue
    , pattern Return
    , pattern If
    , pattern Switch
    , pattern Init
    , pattern Init'
    , pattern Var
    , pattern Scalar
    , pattern Matrix
    , pattern Vector
    , pattern NameAccess
    , pattern RefAccess
    , pattern PointerAccess
    , pattern Assign
    , pattern Call
    , pattern LocalBlock
) where


-- *
import Core

--- Local
import qualified CGIR.Metal.Internal.AST as IR
-- *




-- *
-- | Control Flow Tokens
-- *
pattern Default :: IR.Stmt
pattern Default = IR.DefaultStmt

pattern Break :: IR.Stmt
pattern Break = IR.BreakStmt

pattern Continue :: IR.Stmt
pattern Continue = IR.ContinueStmt

pattern Return :: Maybe IR.Stmt -> IR.Stmt
pattern Return ret = IR.ReturnStmt ret





-- *
-- | Control Flow constructs
-- *

pattern If :: [(IR.Stmt, IR.Block)] -> IR.Block -> IR.Stmt
pattern If intros elseBody = IR.IfStmt intros elseBody


pattern Switch :: IR.Stmt -> [(IR.ScopePath, IR.Block)] -> IR.Stmt
pattern Switch con cases = IR.SwitchStmt con cases








-- *
-- | Iteration Constructs
-- *











-- *
-- | Expression Constructs
-- *


pattern Init :: IR.Type -> IR.Low -> IR.Stmt
pattern Init ty name = IR.InitStmt ty name Nothing


pattern Init' :: IR.Type -> IR.Low -> [IR.InitField] -> IR.Stmt
pattern Init' ty name inits = IR.InitStmt ty name (Just inits)


pattern Var :: IR.Low -> IR.Stmt
pattern Var name = IR.VarStmt name


-- | Values
--
pattern Scalar :: IR.ScalarValue -> IR.Stmt
pattern Scalar val = IR.ScalarStmt val

pattern Matrix :: IR.MatrixValue -> IR.Stmt
pattern Matrix val = IR.MatrixStmt val

pattern Vector :: IR.VectorValue -> IR.Stmt
pattern Vector val = IR.VectorStmt val




pattern NameAccess :: IR.ValuePath -> IR.Stmt
pattern NameAccess path = IR.NameAccessStmt path

pattern RefAccess :: IR.ValuePath -> IR.Stmt
pattern RefAccess path = IR.RefAccessStmt path

pattern PointerAccess :: IR.ValuePath -> IR.Stmt
pattern PointerAccess path = IR.PointerAccessStmt path







-- *
-- | Imperative Constructs
-- *

pattern Assign :: IR.Stmt -> IR.Stmt -> IR.Stmt
pattern Assign id_ expr = IR.AssignStmt id_ expr

pattern Call :: IR.Low -> [IR.Stmt] -> IR.Stmt
pattern Call ref args = IR.CallStmt ref args





-- *
-- | Local (Stmt-Level) Declarations
-- *


pattern LocalBlock :: [IR.Stmt] -> IR.Stmt
pattern LocalBlock body = IR.LocalBlockStmt (IR.Block body)






