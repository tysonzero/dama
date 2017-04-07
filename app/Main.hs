module Main (main) where

import Prelude hiding (lex)

import Data.Monoid ((<>))
import System.Environment (getArgs)

import Dama.Annotator (annotate)
import Dama.Lexer.Lexer (lex)
import Dama.Parser.Parser (parse)

main :: IO ()
main = getArgs >>= \case
    mode : filename : _ -> do
        program <- readFile filename
        let annotated = annotate filename program
            lexed = lex annotated
            parsed = parse =<< lexed
        case mode of
            "annotate" -> print annotated
            "lex" -> print lexed
            "parse" -> print parsed
            _ -> putStrLn $ "invalid mode: " <> mode
    [_] -> putStrLn "no input file"
    [] -> putStrLn "no mode specified"
