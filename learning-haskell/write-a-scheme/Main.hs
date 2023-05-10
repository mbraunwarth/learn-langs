module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

symbols :: Parser Char
symbols = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

readExpr :: String -> String
readExpr input = 
  case parse (spaces >> symbols) "(lisp)" input of
    Left err -> show err
    Right sym -> "Successfully parsed " <> show sym

main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn (readExpr expr)
