module Main where

newtype Parser a = Parser
  { runP :: String -> Maybe (String, a)
  }

instance Functor Parser where
  fmap f (Parser p) = 
    Parser $ \input -> 
      case p input of
        Just (rest, x) -> Just (rest, f x)
        _ -> Nothing

charP :: Char -> Parser Char
charP c = Parser f
  where
    f (x:xs)
      | x == c = Just (xs, x)
      | otherwise = Nothing
    f [] = Nothing

-- stringP :: String -> Parser String
-- stringP = undefined

main :: IO ()
main = do
  let result = runP (charP 'H') "Hello, World!"
  putStrLn (show result)
