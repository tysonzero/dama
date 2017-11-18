module Dama.Parser.AST
    ( Program
    , Decl(PDecl, FDecl)
    , ExprR(ExprRVar, ExprRC)
    , ExprRC(ExprRCons, AppR, ChainR)
    , Expr(ExprIdent, App, Chain, LeftSection, RightSection)
    , Ident(Ident)
    , AltList((:+), (:+:))
    ) where

import Dama.Location

type Program = [Decl]

data Decl
    = PDecl ExprR Expr
    | FDecl Ident [ExprR] Expr
    deriving Show

data ExprR
    = ExprRVar Ident
    | ExprRC ExprRC
    deriving Show

data ExprRC
    = ExprRCons Ident
    | AppR ExprRC ExprR
    | ChainR (AltList ExprR Ident ExprR)
    deriving Show

data Expr
    = ExprIdent Ident
    | App Expr Expr
    | Chain (AltList Expr Ident Expr)
    | LeftSection (AltList Expr Ident Ident)
    | RightSection (AltList Ident Expr Expr)
    deriving Show

data Ident = Ident Location String
    deriving Show

data AltList a b c
    = a :+: b
    | a :+ AltList b a c
infixr 5 :+
infixr 5 :+:

instance (Show a, Show b, Show c) => Show (AltList a b c) where
    showsPrec n (x :+: y) = showParen (n > 5) $ showsPrec 6 x . (" :+: " ++) . showsPrec 6 y
    showsPrec n (x :+ xs) = showParen (n > 5) $ showsPrec 6 x . (" :+ " ++) . showsPrec 5 xs
