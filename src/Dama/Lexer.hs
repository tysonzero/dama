module Dama.Lexer (lex) where

import Prelude hiding (lex)

import Data.Char (isAlpha, isLower, isUpper)
import Data.Monoid ((<>))

import Dama.CharX
import Dama.Location
import Dama.Token

type Lexer = [CharX] -> Either String [Token]

lex :: Lexer
lex ((l, c) : cs)
    | c == ' ' = lex cs
    | c == '\n' = ((l, Newline) :) <$> lex cs
    | c == ':' = lexIdentifier l IdColon (c :) cs
    | isFreeSymbol c = lexIdentifier l IdSymbol (c :) cs
    | isLower c = lexIdentifier l IdLower (c :) cs
    | isUpper c = lexIdentifier l IdUpper (c :) cs
    | otherwise = Left $ unexpected l c
lex [] = Right []

lexIdentifier :: Location -> (String -> Tok) -> (String -> String) -> Lexer
lexIdentifier s t a ((l, c) : cs)
    | c `elem` " \n" = ((s, catchReserved . t $ a []) :) <$> lex cs
    | isAlpha c || isFreeSymbol c = lexIdentifier s t (a . (c :)) cs
    | otherwise = Left $ unexpected l c
lexIdentifier s t a [] = Right [(s, catchReserved . t $ a [])]

catchReserved :: Tok -> Tok
catchReserved (IdSymbol "=") = Equals
catchReserved t = t

isFreeSymbol :: Char -> Bool
isFreeSymbol = (`elem` "!$%&*+-./<=>^")

unexpected :: Location -> Char -> String
unexpected l c = "Unexpected character " <> show c <> " at " <> render l
