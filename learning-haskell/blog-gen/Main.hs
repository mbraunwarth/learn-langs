module Main where

import Html

main :: IO ()
main = putStrLn $ render myhtml

myhtml :: Html
myhtml =
  html_ 
    "My Title" 
    ( append_ 
      (h1_ "Heading") 
      ( append_ 
        (p_ "Paragraph #1") 
        (p_ "Paragraph #2")
      )
    )
