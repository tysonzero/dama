module Dama.Location (Location, render) where

import Data.Monoid ((<>))

type Location = (FilePath, Integer, Integer)

render :: Location -> String
render (f, l, c) = f <> ":" <> show l <> ":" <> show c
