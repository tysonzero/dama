module Dama.Parser.AST
    ( Program
    , Decl(Decl)
    , Expr
    , ExprItem(ExprIdent, SubExpr)
    , Ident(ConsP, ConsI, VarP, VarI)
    ) where

import Data.List.NonEmpty (NonEmpty)

type Program = [Decl]

data Decl = Decl Expr Expr
    deriving Show

type Expr = NonEmpty ExprItem

data ExprItem = ExprIdent Ident | SubExpr Expr
    deriving Show

data Ident
    = ConsP String
    | ConsI String
    | VarP String
    | VarI String
    deriving Show
