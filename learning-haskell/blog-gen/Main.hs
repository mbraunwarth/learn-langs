module Main where

import qualified Html
import qualified Markup
import Convert (convert)

import System.Directory (doesFileExist)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of 
    -- Reading from stdin and write to stdout
    [] -> do 
      contents <- getContents
      putStrLn $ process "Empty Title" contents

    -- Reading from input file and write result to output file
    [inFile, outFile] -> do
      content <- readFile inFile
      exists <- doesFileExist outFile
      let writeResult = writeFile outFile (process inFile content)
      if exists 
        then whenIO confirm writeResult 
        else writeResult

    -- Unallowed argument status
    _ -> 
      putStrLn "Usage: runghc Main.hs [-- <input-file> <output-file>]"

-- | Parse a document to Markup, convert it to Html and render it to a string.
process :: Html.Title -> String -> String
process title = Html.render . convert title . Markup.parse

confirm :: IO Bool
confirm = do
  putStrLn "Are you sure? (y/n)"
  answer <- getLine
  case answer of
    "y" -> pure True
    "n" -> pure False
    _   -> do putStrLn "Please respond with 'y' or 'n'" 
              confirm

whenIO :: IO Bool -> IO () -> IO ()
whenIO cond action = do
  result <- cond
  if result
    then action
    else pure ()
