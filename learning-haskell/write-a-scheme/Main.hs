module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad (liftM)
import System.Environment

data LispVal = Atom String
             | List [LispVal]
             | DottetList [LispVal] LispVal
             | Number Integer
             | String String
             | Char Char
             | Bool Bool
             deriving (Eq, Show)

delimiters :: Parser Char
delimiters = oneOf "[]{}();\"'`|"

symbols :: Parser Char
symbols = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbols
  rest <- many (letter <|> symbols <|> digit)
  let atom = first : rest
  return $ case atom of
    "#f" -> Bool False
    "#t" -> Bool True
    _    -> Atom atom

parseString :: Parser LispVal
parseString = do
  char '"'
  str <- many $ oneOf "\n\t\r" <|> noneOf "\""
  char '"'
  return $ String str

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

-- R5RS Characters: #\<character> or #\<character name>
-- e.g. #\a #\B #\4 #\newline #\space etc.
parseChar :: Parser LispVal
parseChar = do 
  string "#\\"
  first <- char ' ' <|> symbols <|> digit <|> letter
  return $ Char first

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber

readExpr :: String -> String
readExpr input = 
  case parse (parseExpr) "(lisp)" input of
    Left err -> show err
    Right sym -> "Successfully parsed " <> show sym

main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn (readExpr expr)
