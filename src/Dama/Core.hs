module Dama.Core
    ( Program
    , Expr(Var, App, Lam, Case, Let)
    , Decl((:=))
    , Option((:->))
    , Pattern(Bind, Cons)
    , Bind
    ) where

type Program = [Decl]

data Expr
    = Var String
    | App Expr Expr
    | Lam Bind Expr
    | Case Expr [Option]
    | Let [Decl] Expr
    deriving Show

data Decl = String := Expr
    deriving Show
infix 0 :=

data Option = Pattern :-> Expr
    deriving Show
infix 0 :->

data Pattern
    = Bind Bind
    | Cons String [Bind]
    deriving Show

type Bind = Maybe String
