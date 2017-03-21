module Dama.Token
    ( Token
        ( IdLower 
        , IdUpper 
        , IdSymbol
        , IdColon 
        , Equals
        , Newline
        , OpenParen
        , CloseParen
        )
    ) where

data Token
    = IdLower String
    | IdUpper String
    | IdSymbol String
    | IdColon String
    | Equals
    | Newline
    | OpenParen
    | CloseParen
    deriving Show
