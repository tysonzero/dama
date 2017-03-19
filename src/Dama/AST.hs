module Dama.AST
    ( Program(Program)
    , Ident(Prefix, Infix)
    ) where

newtype Program = Program [(String, [Ident])]
    deriving Show

data Ident
    = Prefix String
    | Infix String
    deriving Show
