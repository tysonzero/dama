{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Dama.Parser (parse) where

import Control.Applicative (Alternative, empty, many, (<|>))
import Control.Monad (MonadPlus)
import Control.Monad.Except (Except, MonadError, runExcept, throwError)
import Control.Monad.State (MonadState, StateT, evalStateT, get, put)
import Data.Bifunctor (first)
import Data.Bool (bool)
import Data.List.NonEmpty (NonEmpty((:|)), (<|))
import Data.Monoid ((<>))

import Dama.AST
import Dama.Error
import Dama.Location
import Dama.Token

newtype Parser a = Parser { runParser :: StateT (LocList Token) (Except [Error]) a }
    deriving ( Functor, Applicative, Monad, Alternative, MonadPlus
             , MonadState (LocList Token), MonadError [Error]
             )

instance Monoid (Parser a) where
    mempty = empty
    mappend = (<|>)

parse :: LocList Token -> Either Error Program
parse = first maximum . runExcept . evalStateT (runParser program)

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
    (l, IdLower s) :- _ -> throwError [(l, "Unexpected lower case identifier: " <> s)]
    (l, IdUpper s) :- _ -> throwError [(l, "Unexpected upper case identifier: " <> s)]
    (l, IdSymbol s) :- _ -> throwError [(l, "Unexpected symbol identifier: " <> s)]
    (l, IdColon s) :- _ -> throwError [(l, "Unexpected colon identifier: " <> s)]
    (l, Equals) :- _ -> throwError [(l, "Unexpected equals")]
    (l, Newline) :- _ -> throwError [(l, "Unexpected newline")]
    (l, OpenParen) :- _ -> throwError [(l, "Unexpected open paren")]
    (l, CloseParen) :- _ -> throwError [(l, "Unexpected close paren")]
    Nil l -> throwError [(l, "Unexpected end of input")]
