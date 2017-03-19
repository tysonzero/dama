{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Dama.Parser (parse) where

import Control.Applicative (Alternative, empty, many, some, (<|>))
import Control.Monad (MonadPlus)
import Control.Monad.Except (Except, MonadError, runExcept, throwError)
import Control.Monad.State (MonadState, StateT, evalStateT, get, put)
import Data.Bifunctor (first)
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
program = Program <$> many (many newline *> declaration) <* many newline <* end

declaration :: Parser (String, [Ident])
declaration = (,) <$> idLower <* equals <*> rightHandSide

rightHandSide :: Parser [Ident]
rightHandSide = some (Prefix <$> idLower <> idUpper <|> Infix <$> idSymbol <> idColon)

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
    Nil l -> throwError [(l, "Unexpected end of input")]
