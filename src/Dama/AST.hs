module Dama.AST
    ( Program
    , Decl(Decl)
    , Ident(Prefix, Infix)
    ) where

type Program = [Decl]
    deriving Show

data Decl = Decl String [Ident]
    deriving Show

data Ident
    = Prefix String
    | Infix String
    deriving Show
