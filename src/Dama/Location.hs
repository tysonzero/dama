module Dama.Location (Location, LocList(Nil, (:-))) where

import Data.Monoid ((<>))

type Location = (FilePath, Integer, Integer)

data LocList a = Nil Location | a :- LocList a
infixr 5 :-

instance Show a => Show (LocList a) where
    show (x :- xs) = show x <> " :- " <> show xs
    show (Nil l) = "Nil " <> show l
