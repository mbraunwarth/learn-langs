module Main where

main :: IO ()
main = putStrLn $ render myhtml

newtype Html = Html String
newtype Structure = Structure String
type Title = String

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

html_ :: Title -> Structure -> Html
html_ title (Structure content) = 
  Html 
    ( el "html" 
      ( el "head" (el "title" title) 
        <> el "body" content 
      )
    )

p_ :: String -> Structure
p_ = Structure . el "p"

h1_ :: String -> Structure
h1_ = Structure . el "h1"

el :: String -> String -> String
el tag content = "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

render :: Html -> String
render (Html str) = str

append_ :: Structure -> Structure -> Structure
append_ (Structure s1) (Structure s2) = Structure (s1 <> s2)
