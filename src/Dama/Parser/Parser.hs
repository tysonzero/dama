{-# LANGUAGE GeneralizedNewtypeDeriving, LambdaCase #-}

module Dama.Parser.Parser (parse) where

import Control.Applicative (Alternative, empty, many, (<|>))
import Control.Monad (MonadPlus)
import Control.Monad.State (MonadState, StateT, evalStateT, get, put)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Control.Monad.Writer (MonadWriter, Writer, runWriter, tell)
import Data.List (foldl')
import Data.Monoid ((<>))

import Dama.Error
import Dama.Parser.AST
import Dama.Location
import Dama.Lexer.Token

newtype Parser a = Parser { runParser :: StateT (LocList Token) (MaybeT (Writer Error)) a }
    deriving ( Functor, Applicative, Monad, Alternative, MonadPlus
             , MonadState (LocList Token), MonadWriter Error
             )

instance Monoid (Parser a) where
    mempty = empty
    mappend = (<|>)

parse :: LocList Token -> Either Error Program
parse = toEither . runWriter . runMaybeT . evalStateT (runParser program)
  where
    toEither (mp, e) = maybe (Left e) Right mp

program :: Parser Program
program = (:) <$ many newline <*> declaration <*> program <|> [] <$ end

declaration :: Parser Decl
declaration = PDecl <$> exprR <* equals <*> expr <* newline <> end
          <|> uncurry FDecl <$> funcDecl <* equals <*> expr

funcDecl :: Parser (Ident, [ExprR])
funcDecl = (\(o, as) bs -> (o, as ++ bs)) <$ openParen <*> funcDecl <* closeParen <*> many exprRApp
       <|> (\a o b -> (o, [a, b])) <$> exprR <*> idSymbol <*> exprR
       <|> (,) <$> (idLower <|> openParen *> idSymbol <* closeParen) <*> many exprRApp

exprR :: Parser ExprR
exprR = ExprRVar <$ openParen <*> idSymbol <* closeParen
    <|> ExprRC <$> exprRC
    <|> ExprRVar <$> idLower
    <|> exprRChain

exprRC :: Parser ExprRC
exprRC = ExprRCons <$ openParen <*> idColon <* closeParen
     <|> ChainR <$> chainREE
     <|> exprRCChain

chainREE :: Parser (AltList ExprR Ident ExprR)
chainREE = (:+) <$> exprRChain <*> chainRIE

chainRIE :: Parser (AltList Ident ExprR ExprR)
chainRIE = (:+) <$> idColon <*> chainREE
       <|> (:+:) <$> idColon <*> exprRChain

exprRChain :: Parser ExprR
exprRChain = ExprRC <$> exprRCChain
         <|> exprRApp

exprRCChain :: Parser ExprRC
exprRCChain = foldl' AppR <$> exprRCApp <*> many exprRApp

exprRApp :: Parser ExprR
exprRApp = ExprRC <$> exprRCApp
       <|> ExprRVar <$> idLower
       <|> ExprRVar <$ openParen <*> idSymbol <* closeParen
       <|> openParen *> exprR <* closeParen

exprRCApp :: Parser ExprRC
exprRCApp = ExprRCons <$> idUpper
        <|> ExprRCons <$ openParen <*> idColon <* closeParen
        <|> openParen *> exprRC <* closeParen

expr :: Parser Expr
expr = Chain <$> chainEE
   <|> exprChain

chainEE :: Parser (AltList Expr Ident Expr)
chainEE = (:+) <$> exprChain <*> chainIE

chainEI :: Parser (AltList Expr Ident Ident)
chainEI = (:+) <$> exprChain <*> chainII
      <|> (:+:) <$> exprChain <*> infix_

chainIE :: Parser (AltList Ident Expr Expr)
chainIE = (:+) <$> infix_ <*> chainEE
      <|> (:+:) <$> infix_ <*> exprChain

chainII :: Parser (AltList Ident Expr Ident)
chainII = (:+) <$> infix_ <*> chainEI

exprChain :: Parser Expr
exprChain = foldl' App <$> exprApp <*> many exprApp

exprApp :: Parser Expr
exprApp = ExprIdent <$> prefix
      <|> LeftSection <$ openParen <*> chainEI <* closeParen
      <|> RightSection <$ openParen <*> chainIE <* closeParen
      <|> ExprIdent <$ openParen <*> infix_ <* closeParen
      <|> openParen *> expr <* closeParen

prefix :: Parser Ident
prefix = idLower <> idUpper

infix_ :: Parser Ident
infix_ = idSymbol <> idColon

idLower :: Parser Ident
idLower = get >>= \case
    (l, IdLower s) :- xs -> Ident l s <$ put xs
    _ -> unexpected

idUpper :: Parser Ident
idUpper = get >>= \case
    (l, IdUpper s) :- xs -> Ident l s <$ put xs
    _ -> unexpected

idSymbol :: Parser Ident
idSymbol = get >>= \case
    (l, IdSymbol s) :- xs -> Ident l s <$ put xs
    _ -> unexpected

idColon :: Parser Ident
idColon = get >>= \case
    (l, IdColon s) :- xs -> Ident l s <$ put xs
    _ -> unexpected

equals :: Parser ()
equals = get >>= \case
    (_, Equals) :- xs -> put xs
    _ -> unexpected

newline :: Parser ()
newline = get >>= \case
    (_, Newline) :- xs -> put xs
    _ -> unexpected

openParen :: Parser ()
openParen = get >>= \case
    (_, OpenParen) :- xs -> put xs
    _ -> unexpected

closeParen :: Parser ()
closeParen = get >>= \case
    (_, CloseParen) :- xs -> put xs
    _ -> unexpected

end :: Parser ()
end = get >>= \case
    Nil _ -> pure ()
    _ -> unexpected

unexpected :: Parser a
unexpected = get >>= \case
    (l, IdLower s) :- _ -> tell (Error l $ "Unexpected lower case identifier: " <> s) *> empty
    (l, IdUpper s) :- _ -> tell (Error l $ "Unexpected upper case identifier: " <> s) *> empty
    (l, IdSymbol s) :- _ -> tell (Error l $ "Unexpected symbol identifier: " <> s) *> empty
    (l, IdColon s) :- _ -> tell (Error l $ "Unexpected colon identifier: " <> s) *> empty
    (l, Equals) :- _ -> tell (Error l "Unexpected equals") *> empty
    (l, Newline) :- _ -> tell (Error l "Unexpected newline") *> empty
    (l, OpenParen) :- _ -> tell (Error l "Unexpected open paren") *> empty
    (l, CloseParen) :- _ -> tell (Error l "Unexpected close paren") *> empty
    Nil l -> tell (Error l "Unexpected end of input") *> empty
