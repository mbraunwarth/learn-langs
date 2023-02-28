module Html.Internal where

-- * Types

newtype Html = Html String

newtype Structure = Structure 
  { sContent :: String
  }

instance Semigroup Structure where
  (Structure s1) <> (Structure s2) = Structure (s1 <> s2)

type Title = String

-- * EDSL

html_ :: Title -> Structure -> Html
html_ title structure = 
  let escapedTitle = escape title
  in Html 
    ( el "html" 
      ( el "head" (el "title" escapedTitle) 
        <> el "body" (sContent structure)
      )
    )
  
p_ :: String -> Structure
p_ = Structure . el "p" . escape

h1_ :: String -> Structure
h1_ = Structure . el "h1" . escape

ul_ :: [Structure] -> Structure
ul_ = Structure . el "ul" . concat . map (el "li" . sContent)

ol_ :: [Structure] -> Structure
ol_ = Structure . el "ol" . concat . map (el "li" . sContent)

code_ :: String -> Structure
code_ = Structure . el "pre" . escape

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
