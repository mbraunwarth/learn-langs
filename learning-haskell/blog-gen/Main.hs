module Main where

main :: IO ()
main = putStrLn myhtml

myhtml :: String
myhtml = 
    makeHtml 
        "My page title" 
        (h1_ "Hello, World!" <> p_ "My page content")

makeHtml :: String -> String -> String
makeHtml title body = html_ (head_ (title_ title) <> body_ body)

el :: String -> String -> String
el tag content = "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

html_ :: String -> String
html_ = el "html"

body_ :: String -> String
body_ content = el "body" content

head_ :: String -> String
head_ content = el "head" content

title_ :: String -> String
title_ content = el "title" content

p_ :: String -> String
p_ content = el "p" content

h1_ :: String -> String
h1_ content = el "h1" content
