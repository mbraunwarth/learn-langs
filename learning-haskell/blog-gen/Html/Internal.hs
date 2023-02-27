module Html.Internal where

-- * Types

newtype Html = Html String
newtype Structure = Structure String
type Title = String

-- * EDSL

html_ :: Title -> Structure -> Html
html_ title (Structure content) = 
  let escapedTitle = escape title
  in Html 
    ( el "html" 
      ( el "head" (el "title" escapedTitle) 
        <> el "body" content 
      )
    )
  
p_ :: String -> Structure
p_ = Structure . el "p" . escape

h1_ :: String -> Structure
h1_ = Structure . el "h1" . escape

append_ :: Structure -> Structure -> Structure
append_ (Structure s1) (Structure s2) = Structure (s1 <> s2)

-- * Render

render :: Html -> String
render (Html str) = str

-- * Utilities

el :: String -> String -> String
el tag content = "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

escape :: String -> String
escape = 
  let 
    escapeChar c = case c of
      '&' -> "&amp;"
      '>' -> "&lt;"
      '<' -> "&gt;"
      '"' -> "&quot;"
      '\'' -> "&#39;"
      _ -> [c]
  in concat . map escapeChar
