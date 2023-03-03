module Main where

import qualified Html
import qualified Markup
import Convert (convert)

import System.Directory (doesFileExist)
import System.Environment (getArgs)

main :: IO ()
main = 
  getArgs >>= 
    \args ->
      case args of
        -- Reading from stdin and write to stdout
        [] -> getContents >>= putStrLn . process "Empty Title"

        -- Reading from input file and write result to output file
        [inFile, outFile] -> 
          readFile inFile >>= \content ->
            doesFileExist outFile >>= \exists ->
              let 
                writeResult = writeFile outFile (process inFile content)
              in 
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
confirm = 
  putStrLn "Are you sure? (y/n)" *>
    getLine >>= \answer ->
      case answer of
        "y" -> pure True
        "n" -> pure False
        _   -> putStrLn "Invalid response. Use y or n" *>
                 confirm

whenIO :: IO Bool -> IO () -> IO ()
whenIO cond action =
  cond >>= \result ->
    if result
      then action
      else pure ()
