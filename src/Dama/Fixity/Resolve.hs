{-# LANGUAGE LambdaCase #-}

module Dama.Fixity.Resolve (resolveFixity) where

import Data.Set (Set)
import qualified Data.Set as S

import Dama.Error
import qualified Dama.Fixity.IR as IR
import Dama.Location
import Dama.Parser.AST (AltList((:+), (:+:)))
import qualified Dama.Parser.AST as AST

resolveFixity :: AST.Program -> Either Error IR.Program
resolveFixity = traverse resolveDecl

resolveDecl :: AST.Decl -> Either Error IR.Decl
resolveDecl (AST.PDecl a b) = IR.PDecl <$> resolveExprR a <*> resolveExpr b
resolveDecl (AST.FDecl f as b) = IR.FDecl (resolveIdent f)
    <$> traverse resolveExprR as <*> resolveExpr b

resolveExprR :: AST.ExprR -> Either Error IR.ExprR
resolveExprR (AST.ExprRVar i) = Right . IR.ExprRVar $ resolveIdent i
resolveExprR (AST.ExprRC a) = uncurry IR.ExprRCons <$> resolveExprRC a

resolveExprRC :: AST.ExprRC -> Either Error (IR.Ident, [IR.ExprR])
resolveExprRC (AST.ExprRCons i) = Right (resolveIdent i, [])
resolveExprRC (AST.AppR a b) = (\(i, cs) d -> (i, cs ++ [d])) <$> resolveExprRC a <*> resolveExprR b
resolveExprRC (AST.ChainR c) = resolveChainR c

resolveChainR :: AltList AST.ExprR AST.Ident AST.ExprR -> Either Error (IR.Ident, [IR.ExprR])
resolveChainR = \case
    a :+ o :+: b -> resolveExprRC $ convert a o b
    a :+ o :+ b :+ cs -> resolveChainR $ AST.ExprRC (convert a o b) :+ cs
  where
    convert a o = AST.AppR $ AST.AppR (AST.ExprRCons o) a


resolveExpr :: AST.Expr -> Either Error IR.Expr
resolveExpr (AST.ExprIdent i) = Right . IR.ExprIdent $ resolveIdent i
resolveExpr (AST.App a b) = IR.App <$> resolveExpr a <*> resolveExpr b
resolveExpr (AST.Chain c) = resolveChain c

resolveExpr (AST.LeftSection c) = IR.Lam (IR.Ident location varName)
    <$> resolveChain (appendEI c)
  where
    appendEI :: AltList AST.Expr AST.Ident AST.Ident -> AltList AST.Expr AST.Ident AST.Expr
    appendEI (a :+: b@(AST.Ident l _)) = a :+ b :+: AST.ExprIdent (AST.Ident l varName)
    appendEI (a :+ bs) = a :+ appendII bs

    appendII :: AltList AST.Ident AST.Expr AST.Ident -> AltList AST.Ident AST.Expr AST.Expr
    appendII (a :+ bs) = a :+ appendEI bs

    location = case c of
        a :+: _ -> exprLocation a
        a :+ _ -> exprLocation a

    varName = freshVar $ chainEIVars c

resolveExpr (AST.RightSection c) = IR.Lam (IR.Ident location varName)
    <$> resolveChain (AST.ExprIdent (AST.Ident location varName) :+ c)
  where
    location = case c of
        AST.Ident l _ :+: _ -> l
        AST.Ident l _ :+ _ -> l

    varName = freshVar $ chainIEVars c


resolveChain :: AltList AST.Expr AST.Ident AST.Expr -> Either Error IR.Expr
resolveChain = \case
    a :+ o :+: b -> resolveExpr $ convert a o b
    a :+ o :+ b :+ cs -> resolveChain $ convert a o b :+ cs
  where
    convert a o = AST.App $ AST.App (AST.ExprIdent o) a

resolveIdent :: AST.Ident -> IR.Ident
resolveIdent (AST.Ident l s) = IR.Ident l s

exprLocation :: AST.Expr -> Location
exprLocation (AST.ExprIdent (AST.Ident l _)) = l
exprLocation (AST.App a _) = exprLocation a
exprLocation (AST.Chain (a :+ _)) = exprLocation a
exprLocation (AST.LeftSection (a :+ _)) = exprLocation a
exprLocation (AST.LeftSection (a :+: _)) = exprLocation a
exprLocation (AST.RightSection (AST.Ident l _ :+ _)) = l
exprLocation (AST.RightSection (AST.Ident l _ :+: _)) = l

freshVar :: Set String -> String
freshVar vs = head . dropWhile (`S.member` vs) $ ('v' :) . show <$> [0 :: Integer ..]

exprVars :: AST.Expr -> Set String
exprVars (AST.ExprIdent (AST.Ident _ s)) = S.singleton s
exprVars (AST.App a b) = S.union (exprVars a) (exprVars b)
exprVars (AST.Chain c) = chainEEVars c
exprVars (AST.LeftSection c) = chainEIVars c
exprVars (AST.RightSection c) = chainIEVars c

chainEEVars :: AltList AST.Expr AST.Ident AST.Expr -> Set String
chainEEVars (a :+ as) = S.union (exprVars a) (chainIEVars as)

chainEIVars :: AltList AST.Expr AST.Ident AST.Ident -> Set String
chainEIVars (a :+: AST.Ident _ s) = S.insert s $ exprVars a
chainEIVars (a :+ as) = S.union (exprVars a) (chainIIVars as)

chainIEVars :: AltList AST.Ident AST.Expr AST.Expr -> Set String
chainIEVars (AST.Ident _ s :+: a) = S.insert s $ exprVars a
chainIEVars (AST.Ident _ s :+ as) = S.insert s $ chainEEVars as

chainIIVars :: AltList AST.Ident AST.Expr AST.Ident -> Set String
chainIIVars (AST.Ident _ s :+ as) = S.insert s $ chainEIVars as
