module Dama.Lexer (lex) where

import Prelude hiding (lex)

import Data.Char (isAlpha, isLower, isUpper)
import Data.Monoid ((<>))

import Dama.CharX
import Dama.Error
import Dama.Location
import Dama.Token

type Lexer = LocList CharX -> Either Error (LocList Token)

lex :: Lexer
lex ((l, c) :- cs)
    | c == ' ' = lex cs
    | c == '\n' = ((l, Newline) :-) <$> lex cs
    | c == ':' = lexIdentifier l IdColon (c :) cs
    | isFreeSymbol c = lexIdentifier l IdSymbol (c :) cs
    | isLower c = lexIdentifier l IdLower (c :) cs
    | isUpper c = lexIdentifier l IdUpper (c :) cs
    | otherwise = Left (l, "Unexpected character " <> show c)
lex (Nil l) = Right $ Nil l

lexIdentifier :: Location -> (String -> Tok) -> (String -> String) -> Lexer
lexIdentifier s t a ((l, c) :- cs)
    | c `elem` " \n" = ((s, catchReserved . t $ a []) :-) <$> lex ((l, c) :- cs)
    | isAlpha c || isFreeSymbol c = lexIdentifier s t (a . (c :)) cs
    | otherwise = Left (l, "Unexpected character " <> show c)
lexIdentifier s t a (Nil l) = Right $ (s, catchReserved . t $ a []) :- Nil l

catchReserved :: Tok -> Tok
catchReserved (IdSymbol "=") = Equals
catchReserved t = t

isFreeSymbol :: Char -> Bool
isFreeSymbol = (`elem` "!$%&*+-./<=>^")
