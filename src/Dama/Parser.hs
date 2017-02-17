{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Dama.Parser (parse) where

import Control.Applicative (Alternative)
import Control.Monad (MonadPlus)
import Control.Monad.Except (Except, MonadError, runExcept)
import Control.Monad.State (MonadState, StateT, evalStateT)
import Data.Bifunctor (first)

import Dama.AST
import Dama.Error
import Dama.Location
import Dama.Token

newtype Parser a = Parser { runParser :: StateT (LocList Token) (Except [Error]) a }
    deriving ( Functor, Applicative, Monad, Alternative, MonadPlus
             , MonadState (LocList Token), MonadError [Error]
             )

parse :: LocList Token -> Either Error Program
parse = first maximum . runExcept . evalStateT (runParser program)

program :: Parser Program
program = error "program"
