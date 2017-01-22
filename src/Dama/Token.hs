module Dama.Token
    ( Token
    , Tok
        ( ConsP
        , ConsI
        , VarP
        , VarI
        , Assign
        , Newline
        )
    ) where

import Dama.Location

type Token = (Location, Tok)

data Tok
    = ConsP String
    | ConsI String
    | VarP String
    | VarI String
    | Assign
    | Newline
    deriving (Eq, Ord, Show)
