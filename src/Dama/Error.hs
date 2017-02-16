module Dama.Error (Error) where

import Dama.Location

type Error = (Location, String)
