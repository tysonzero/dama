module Dama.AST
    ( Program(Program)
    , Decl(Decl)
    , Ident(Prefix, Infix)
    ) where

newtype Program = Program [Decl]
    deriving Show

data Decl = Decl String [Ident]
    deriving Show

data Ident
    = Prefix String
    | Infix String
    deriving Show
