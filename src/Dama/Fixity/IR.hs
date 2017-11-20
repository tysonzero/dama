module Dama.Fixity.IR
    ( Program
    , Decl(PDecl, FDecl)
    , ExprR(ExprRVar, ExprRCons)
    , Expr(ExprIdent, App, Lam)
    , Ident(Ident)
    ) where

import Dama.Location

type Program = [Decl]

data Decl
    = PDecl ExprR Expr
    | FDecl Ident [ExprR] Expr
    deriving Show

data ExprR
    = ExprRVar Ident
    | ExprRCons Ident [ExprR]
    deriving Show

data Expr
    = ExprIdent Ident
    | App Expr Expr
    | Lam Ident Expr
    deriving Show

data Ident = Ident Location String
    deriving Show
