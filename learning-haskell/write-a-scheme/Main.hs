module Main where

import Control.Monad (liftM)
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Character Char
             | Number Integer
             | String String
             | Bool Bool
             deriving (Eq, Show)

main :: IO ()
main = do 
    args <- getArgs
    putStrLn $ readExpr (args !! 0)

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value: " ++ show val

parseExpr :: Parser LispVal
parseExpr = parseAtom 
        <|> parseChar
        <|> parseString
        <|> parseNumber

parseString :: Parser LispVal
parseString = do
    char '"'
    str <- many (escape <|> digit <|> letter <|> space)
    char '"'
    return $ String str

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = [first] ++ rest
    return $ case atom of
                "#t" -> Bool True
                "#f" -> Bool False
                otherwise -> Atom atom
    
parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseChar :: Parser LispVal
parseChar = do
    string "#\\"
    c <- (oneOf "()[]{}" <|> symbol <|> alphaNum)
    return $ Character c

symbol :: Parser Char
symbol = oneOf "!§$%&/+-*:<>|^@_~#="

spaces :: Parser ()
spaces = skipMany1 space

escape :: Parser Char
escape = oneOf "\"\t\n\r\\"
