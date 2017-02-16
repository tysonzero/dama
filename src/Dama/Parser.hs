{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Dama.Parser (parse) where

import Control.Applicative (Alternative, empty, (<|>))
import Control.Monad (MonadPlus)
import Control.Monad.Except (Except, MonadError, runExcept)
import Control.Monad.State (MonadState, StateT, evalStateT)

import Dama.AST
import Dama.Error
import Dama.Token

newtype Parser a = Parser { runParser :: StateT [Token] (Except [Error]) a }
    deriving ( Functor, Applicative, Monad, Alternative, MonadPlus
             , MonadState [Token], MonadError [Error]
             )

instance Monoid (Parser a) where
    mempty = empty
    mappend = (<|>)

parse :: [Token] -> Either [Error] Program
parse = runExcept . evalStateT (runParser program)

program :: Parser Program
program = error "program"
