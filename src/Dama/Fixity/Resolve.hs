{-# LANGUAGE TypeApplications #-}

module Dama.Fixity.Resolve (resolveFixity) where

import Data.List.NonEmpty (NonEmpty((:|)), (<|))
import qualified Data.List.NonEmpty as NE
import Data.Monoid ((<>))
import Data.Set (Set)
import qualified Data.Set as S

import Dama.Error
import qualified Dama.Parser.AST as AST
import qualified Dama.Fixity.IR as IR

resolveFixity :: AST.Program -> Either Error IR.Program
resolveFixity = traverse resolveDecl

resolveDecl :: AST.Decl -> Either Error IR.Decl
resolveDecl (AST.Decl l r) = uncurry IR.Decl <$> resolveExprR l <*> resolveExpr r

resolveExprR :: AST.ExprR -> Either Error (String, [IR.Pattern])
resolveExprR _ = error "resolveExprR"

resolveExpr :: AST.Expr -> Either Error IR.Expr
resolveExpr (x :| xs) = case x :| xs of
    AST.ExprIdent (AST.Infix _ o) :| y : ys -> do
        e <- resolveExpr $ y :| ys
        let v = getFresh "secR" $ variables e
        go (Just (Right . IR.Lam v $ IR.App (IR.App (IR.Lit o) (IR.Lit v)) e, True)) (x :| []) xs
    AST.ExprIdent (AST.Infix _ o) :| [] -> Right $ IR.Lit o
    _ -> go Nothing (x :| []) xs
  where
    go (Just (_, True)) _ (AST.ExprIdent (AST.Infix l _) : _)
        = Left $ Error l "Section operator must have lowest precedence"
    go _ ps (AST.ExprIdent (AST.Infix l o) : fs) = case fs of
        f : fs' -> go ( Just
                ( IR.App . IR.App (IR.Lit o) <$> resolveExpr (NE.reverse ps)
                    <*> resolveExpr (f :| fs')
                , False ) )
            (AST.ExprIdent (AST.Infix l o) <| ps) fs
        [] -> do
            e <- resolveExpr $ NE.reverse ps
            let v = getFresh "secL" $ variables e
            pure . IR.Lam v . flip IR.App (IR.Lit v) $ IR.App (IR.Lit o) e
    go e ps (f : fs) = go e (f <| ps) fs
    go (Just (e, _)) _ [] = e
    go Nothing (p :| (p' : ps)) []
        = IR.App <$> resolveExpr (NE.reverse $ p' :| ps) <*> resolveExprItem p
    go Nothing (p :| []) [] = resolveExprItem p

resolveExprItem :: AST.ExprItem -> Either Error IR.Expr
resolveExprItem (AST.ExprIdent (AST.Prefix _ n)) = Right $ IR.Lit n
resolveExprItem (AST.ExprIdent (AST.Infix _ n)) = Right $ IR.Lit n
resolveExprItem (AST.SubExpr e) = resolveExpr e

getFresh :: String -> Set String -> String
getFresh n vs = head . dropWhile (`S.member` vs) . (n :) $ (n ++) . show @Integer <$> [1 ..]

variables :: IR.Expr -> Set String
variables (IR.Lit n) = S.singleton n
variables (IR.App f x) = variables f <> variables x
variables (IR.Lam n e) = S.singleton n <> variables e