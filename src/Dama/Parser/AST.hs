module Dama.Parser.AST
    ( Program
    , Decl(Decl)
    , ExprR
    , ExprItemR(ExprIdentR, SubExprR)
    , IdentR(ConsP, ConsI, VarP, VarI)
    , Expr
    , ExprItem(ExprIdent, SubExpr)
    , Ident(Prefix, Infix)
    ) where

import Data.List.NonEmpty (NonEmpty)

type Program = [Decl]

data Decl = Decl ExprR Expr
    deriving Show

type ExprR = NonEmpty ExprItemR

data ExprItemR = ExprIdentR IdentR | SubExprR ExprR
    deriving Show

data IdentR
    = ConsP String
    | ConsI String
    | VarP String
    | VarI String
    deriving Show

type Expr = NonEmpty ExprItem

data ExprItem = ExprIdent Ident | SubExpr Expr
    deriving Show

data Ident
    = Prefix String
    | Infix String
    deriving Show
