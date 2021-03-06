module Dama.Lexer.Lexer (Lexer, lex) where

import Prelude hiding (lex)

import Data.Char (isAlpha, isDigit, isLower, isUpper)
import Data.Monoid ((<>))

import Dama.Error
import Dama.Location
import Dama.Lexer.Token

type Lexer = LocList Char -> Either Error (LocList Token)

lex :: Lexer
lex ((l, c) :- cs)
    | c == ' ' = lex cs
    | c == '\n' = ((l, Newline) :-) <$> lex cs
    | c == '(' = ((l, OpenParen) :-) <$> lex cs
    | c == ')' = ((l, CloseParen) :-) <$> lex cs
    | c == ':' = lexIdentifier l IdColon (c :) cs
    | isFreeSymbol c = lexIdentifier l IdSymbol (c :) cs
    | isLower c = lexIdentifier l IdLower (c :) cs
    | isUpper c = lexIdentifier l IdUpper (c :) cs
    | otherwise = Left . Error l $ "Unexpected character: " <> show c
lex (Nil l) = Right $ Nil l

lexIdentifier :: Location -> (String -> Token) -> (String -> String) -> Lexer
lexIdentifier s t a ((l, c) :- cs)
    | c `elem` " \n()" = ((s, catchReserved . t $ a []) :-) <$> lex ((l, c) :- cs)
    | isAlpha c || c == ':' || isFreeSymbol c || isDigit c = lexIdentifier s t (a . (c :)) cs
    | otherwise = Left . Error l $ "Unexpected character: " <> show c
lexIdentifier s t a (Nil l) = Right $ (s, catchReserved . t $ a []) :- Nil l

catchReserved :: Token -> Token
catchReserved (IdSymbol "=") = Equals
catchReserved t = t

isFreeSymbol :: Char -> Bool
isFreeSymbol = (`elem` "!$%&*+-./<=>^")
