{-# LANGUAGE NoImplicitPrelude #-}
module Core.Text.Util (
    punctuate
) where



import qualified Data.Text as Text


punctuate :: Text.Text -> [Text.Text] -> [Text.Text]
punctuate _ []     = []
punctuate _ [x]    = [x]
punctuate p (x:xs) =
        (x `Text.append` p) : punctuate p xs

