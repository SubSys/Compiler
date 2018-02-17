{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module GCIR.RustCG.AST.Data.Semantic.Base.Ident (
    IR.Ident
  , IR.Path
  , IR.Seg
  , IR.Prefix(..)

  , pattern Ident
  , pattern Path
  , pattern Seg
  , pattern Path_
  , pattern Seg_
) where


-- *
import Core

--- Local
import qualified GCIR.RustCG.Internal.AST as IR
-- *


pattern Ident :: Text -> IR.Ident
pattern Ident text = IR.Ident text


-- | Paths & Path Segments
-- 
pattern Path :: [IR.Seg] -> IR.Path
pattern Path segments = IR.Path segments

pattern Seg :: Maybe IR.Prefix -> Text -> IR.Seg
pattern Seg prefix text = IR.Seg prefix text

-- | Segment Prefixes & Postfixes
-- 

-- TODO: ...


-- | Alternate variations - convenience helpers
--


pattern Path_ :: [Text] -> IR.Path
pattern Path_ segments <- IR.Path (segs2Text -> segments)
    where
        Path_ segments = IR.Path (texts2Seg segments)


pattern Seg_ :: Text -> IR.Seg
pattern Seg_ text = IR.Seg Nothing text




-- | Internal Helpers
--

segs2Text :: [IR.Seg] -> [Text]
segs2Text = map seg2Text

seg2Text :: IR.Seg -> Text
seg2Text (IR.Seg _ x) = x

texts2Seg :: [Text] -> [IR.Seg]
texts2Seg = map text2Seg

text2Seg :: Text -> IR.Seg
text2Seg = IR.Seg Nothing


