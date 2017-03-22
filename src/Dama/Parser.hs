{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Dama.Parser (parse) where

import Control.Applicative (Alternative, empty, many, (<|>))
import Control.Monad (MonadPlus)
import Control.Monad.State (MonadState, StateT, evalStateT, get, put)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Control.Monad.Writer (MonadWriter, Writer, runWriter, tell)
import Data.Bool (bool)
import Data.List.NonEmpty (NonEmpty((:|)), (<|))
import Data.Monoid ((<>))

import Dama.AST
import Dama.Error
import Dama.Location
import Dama.Token

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
    toEither (Just p, _) = Right p
    toEither (Nothing, e) = Left e

program :: Parser Program
program = many newline *> ((:) <$> declaration <*> program) <> ([] <$ end)

declaration :: Parser Decl
declaration = Decl <$> idLower <* equals <*> expr False False <* newline <> end

expr :: Bool -> Bool -> Parser Expr
expr l r = (<|) <$> prefixPart <*> expr True r
       <|> (<|) <$> bool prefixPart anyPart l <*> expr False r
       <|> (:| []) <$> bool prefixPart anyPart (l && r)
  where
    anyPart = ExprIdent . Infix <$> idSymbol <> idColon <|> prefixPart
    prefixPart = ExprIdent . Prefix <$> idLower <> idUpper
             <|> SubExpr <$ openParen <*> parenExpr <* closeParen
    parenExpr = expr False True <|> expr True False <|> (:| []) <$> anyPart

idLower :: Parser String
idLower = get >>= \case
    (_, IdLower s) :- xs -> put xs *> pure s
    _ -> unexpected

idUpper :: Parser String
idUpper = get >>= \case
    (_, IdUpper s) :- xs -> put xs *> pure s
    _ -> unexpected

idSymbol :: Parser String
idSymbol = get >>= \case
    (_, IdSymbol s) :- xs -> put xs *> pure s
    _ -> unexpected

idColon :: Parser String
idColon = get >>= \case
    (_, IdColon s) :- xs -> put xs *> pure s
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
