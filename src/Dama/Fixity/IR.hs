module Dama.Fixity.IR
    ( Program
    , Decl(Decl)
    , Pattern(Bind, Cons)
    , Expr(App)
    ) where

type Program = [Decl]

data Decl = Decl String [Pattern] Expr

data Pattern
    = Bind String
    | Cons String [Pattern]

data Expr = App String [Expr]
