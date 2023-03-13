module HsBlog.Html.Internal where

import Numeric.Natural

-- * Types

newtype Html = Html String
  deriving (Show)

newtype Structure = Structure 
  { sContent :: String
  }
  deriving (Show)

instance Semigroup Structure where
  (Structure s1) <> (Structure s2) = Structure (s1 <> s2)

instance Monoid Structure where
  mempty = empty_

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

h_ :: Natural -> String -> Structure
h_ n = Structure . el ("h" <> show n) . escape

h1_ :: String -> Structure
h1_ = h_ 1

h2_ :: String -> Structure
h2_ = h_ 1

h3_ :: String -> Structure
h3_ = h_ 1

h4_ :: String -> Structure
h4_ = h_ 1

h5_ :: String -> Structure
h5_ = h_ 1

h6_ :: String -> Structure
h6_ = h_ 1

ul_ :: [Structure] -> Structure
ul_ = Structure . el "ul" . concat . map (el "li" . sContent)

ol_ :: [Structure] -> Structure
ol_ = Structure . el "ol" . concat . map (el "li" . sContent)

code_ :: String -> Structure
code_ = Structure . el "pre" . escape

empty_ :: Structure
empty_ = Structure ""

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
      '>' -> "&gt;"
      '<' -> "&lt;"
      '"' -> "&quot;"
      '\'' -> "&#39;"
      _ -> [c]
  in concat . map escapeChar
