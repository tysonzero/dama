module Dama.Location (Location, LocList(Nil, (:-)), render) where

import Data.Monoid ((<>))

type Location = (FilePath, Integer, Integer)

data LocList a = Nil Location | a :- LocList a
infixr 5 :-

render :: Location -> String
render (f, l, c) = f <> ":" <> show l <> ":" <> show c
