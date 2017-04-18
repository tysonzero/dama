module Dama.Fixity.IR
    ( Program
    , Decl(Decl)
    , Pattern(Bind, Cons)
    , Expr(Lit, App, Lam)
    ) where

type Program = [Decl]

data Decl = Decl String [Pattern] Expr
    deriving Show

data Pattern
    = Bind String
    | Cons String [Pattern]
    deriving Show

data Expr
    = Lit String
    | App Expr Expr
    | Lam String Expr
    deriving Show
