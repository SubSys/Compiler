{-# LANGUAGE NoImplicitPrelude #-}
module SLIR.HelmSyntax.AST.Data.TopLevel.Unions.Utils (
      lookupUnion
    , lookupConstructor
    , lookupUnionCon
) where


-- *
import Core

import qualified Data.List as List

--- Local
import qualified SLIR.HelmSyntax.Internal.AST as IR

-- ~ AST - Essential Instances
import SLIR.HelmSyntax.Internal.AST.Instances.Essential ()
-- *

-- | Internal
type ConstructorName = IR.Big Text


lookupUnion :: ConstructorName -> [IR.Union] -> Maybe IR.Union
lookupUnion name =
    List.find check
    where
        
        check union@(IR.Union _ _ cs meta) =
            List.any checkInner cs
        
        checkInner (IR.Constructor name' args meta)
            | name' == name = True
            | otherwise     = False



lookupConstructor :: ConstructorName -> IR.Union -> Maybe IR.Constructor
lookupConstructor name (IR.Union _ _ cs _) =
    List.find check cs
    where
        check (IR.Constructor name' args meta)
            | name' == name = True
            | otherwise     = False



lookupUnionCon :: ConstructorName -> [IR.Union] -> Maybe (IR.Union, IR.Constructor)
lookupUnionCon name unions =
    case lookupUnion name unions of
        Nothing -> Nothing
        Just union -> case lookupConstructor name union of
            Nothing -> Nothing
            Just con ->
                Just (union, con)

