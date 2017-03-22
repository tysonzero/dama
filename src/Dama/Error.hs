module Dama.Error (Error(Error, UnknownError)) where

import Dama.Location

data Error = UnknownError | Error Location String
    deriving Show

instance Monoid Error where
    mempty = UnknownError
    Error la sa `mappend` Error lb sb
        | lb > la = Error lb sb
        | otherwise = Error la sa
    Error l s `mappend` _ = Error l s
    _ `mappend` Error l s = Error l s
    _ `mappend` _ = UnknownError
