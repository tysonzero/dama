module Dama.Token
    ( Token
    , Tok
        ( IdLower 
        , IdUpper 
        , IdSymbol
        , IdColon 
        , Equals
        , Newline
        )
    ) where

import Dama.Location

type Token = (Location, Tok)

data Tok
    = IdLower String
    | IdUpper String
    | IdSymbol String
    | IdColon String
    | Equals
    | Newline
