module Dama.Core.Render (renderCore) where

import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Dama.Core.Core

renderCore :: Program -> String
renderCore ds = unwords $ renderDec <$> ds
  where
    renderDec (n := e) = n <> " = " <> renderExpr e

renderExpr :: Expr -> String
renderExpr (Var s) = s
renderExpr (App f x) = renderExpr f <> " " <> renderExpr x
renderExpr (Lam b e) = "\\ " <> renderBind b <> " -> " <> renderExpr e
renderExpr (Case e os) = renderExpr e <> " => { " <> intercalate " | " (renderOption <$> os) <> " }"
renderExpr (Let ds e) = "{ " <> intercalate " | " (renderDecl <$> ds) <> " } => " <> renderExpr e

renderDecl :: Decl -> String
renderDecl (n := e) = n <> " = " <> renderExpr e

renderOption :: Option -> String
renderOption (p :-> e) = renderPattern p <> " -> " <> renderExpr e

renderPattern :: Pattern -> String
renderPattern (Bind b) = renderBind b
renderPattern (Cons c bs) = c <> ((' ' :) . renderBind =<< bs)

renderBind :: Bind -> String
renderBind = fromMaybe "_" 
