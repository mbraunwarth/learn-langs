module Main where

import Html
import Convert

main :: IO ()
main = putStrLn $ render myhtml

myhtml :: Html
myhtml =
  html_ 
    "My Title" 
    ( h1_ "Heading" <>
      ( p_ "Paragraph #1" <> p_ "Paragraph #2" )
    )
