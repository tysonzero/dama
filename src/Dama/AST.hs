module Dama.AST
    ( Program
    , Decl(Decl)
    , Expr
    , ExprItem(ExprIdent, SubExpr)
    , Ident(Prefix, Infix)
    ) where

import Data.List.NonEmpty (NonEmpty)

type Program = [Decl]

data Decl = Decl String Expr
    deriving Show

type Expr = NonEmpty ExprItem

data ExprItem = ExprIdent Ident | SubExpr Expr
    deriving Show

data Ident
    = Prefix String
    | Infix String
    deriving Show
