module HsBlog.Markup where

import Data.Maybe
import Numeric.Natural

type Document 
  = [Structure]

data Structure
  = Heading Natural String
  | Paragraph String
  | UnorderedList [String]
  | OrderedList [String]
  | CodeBlock [String]
  deriving(Show, Eq)

parse :: String -> Document
parse = parseLines Nothing . lines

parseLines :: Maybe Structure -> [String] -> Document
parseLines context txts =
  case txts of
    -- base case
    [] -> maybeToList context

    -- Heading 1
    ('*' : ' ' : line) : rest ->
      maybe id (:) context (Heading 1 (trim line) : (parseLines Nothing rest))

    -- Unordered List
    ('-' : ' ' : line) : rest ->
      case context of
        Just (UnorderedList list) ->
          parseLines (Just $ UnorderedList (list <> [(trim line)])) rest
        _ -> 
          maybe id (:) context (parseLines (Just $ UnorderedList [trim line]) rest)

    -- Ordered List
    ('#' : ' ' : line) : rest ->
      case context of
      Just (OrderedList list) ->
        parseLines (Just $ OrderedList (list <> [trim line])) rest
      _ ->
        maybe id (:) context (parseLines (Just $ OrderedList [trim line]) rest)

    -- Code Blocks
    ('>' : ' ' : line) : rest ->
      case context of
        Just (CodeBlock block) ->
          parseLines (Just $ CodeBlock (block <> [trim line])) rest
        _ -> 
          maybe id (:) context (parseLines (Just $ CodeBlock [trim line]) rest)

    -- Paragraph
    currentLine : rest ->
      let line = trim currentLine
      in if line == ""
           then
             maybe id (:) context (parseLines Nothing rest)
           else
              case context of
                Just (Paragraph paragraph) ->
                  parseLines (Just $ Paragraph (unwords [paragraph, line])) rest
                _ ->
                  maybe id (:) context (parseLines (Just $ Paragraph line) rest)

trim :: String -> String
trim = unwords . words
