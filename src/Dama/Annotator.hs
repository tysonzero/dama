module Dama.Annotator (annotate) where

import Dama.Location

annotate :: FilePath -> String -> LocList Char
annotate f s = go s 1 1
  where
    go ('\n' : xs) l c = ((f, l, c), '\n') :- go xs (l + 1) 0
    go (x : xs) l c = ((f, l, c), x) :- go xs l (c + 1)
    go [] l c = Nil (f, l, c)
