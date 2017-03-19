module Dama.Token
    ( Token
        ( IdLower 
        , IdUpper 
        , IdSymbol
        , IdColon 
        , Equals
        , Newline
        )
    ) where

data Token
    = IdLower String
    | IdUpper String
    | IdSymbol String
    | IdColon String
    | Equals
    | Newline
    deriving Show
