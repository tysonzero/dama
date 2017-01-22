module Dama.Annotator (annotate) where

import Dama.CharX (CharX)

annotate :: FilePath -> String -> [CharX]
annotate f s = go s 1 1
  where
    go ('\n' : xs) l c = ((f, l, c), '\n') : go xs (l + 1) 0
    go (x : xs) l c = ((f, l, c), x) : go xs l (c + 1)
    go [] _ _ = []
