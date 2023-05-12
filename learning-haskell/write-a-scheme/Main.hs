module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

data LispVal = Atom String
             | List [LispVal]
             | DottetList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             deriving (Eq, Show)

symbols :: Parser Char
symbols = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do
  char '"'
  str <- many $ noneOf "\""
  char '"'
  return $ String str

readExpr :: String -> String
readExpr input = 
  case parse (spaces >> symbols) "(lisp)" input of
    Left err -> show err
    Right sym -> "Successfully parsed " <> show sym

main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn (readExpr expr)
