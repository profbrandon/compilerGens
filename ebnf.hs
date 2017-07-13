import System.Environment
import System.Directory
import Data.List

import EBNF.Lexer

main :: IO ()
main =
  do
    args <- getArgs
    bools <- mapM (doesFileExist) args
    let exists = and (bools)
    if exists
      then
        do
          out <- lexFiles args
          mapM (printOut) out
          putStr ""
      else error "Not all files exist as given"

printOut :: (String, String, [Token]) -> IO ()
printOut (filename, file, ts) =
  let isError e    = case e of Err _ _ _ -> True; _ -> False
      (errs, toks) = partition (isError) ts
  in do
    putStrLn $ filename ++ ":----------"
    putStrLn $ showTokens toks
    putStrLn "-----------------"
    mapM (\t -> putStrLn (showError t file)) errs
    putStrLn ""

lexFiles :: [String] -> IO [(String, String, [Token])]
lexFiles [] = error "No files provided"
lexFiles (x:xs)
  | xs == [] =
    do
      content <- readFile x
      return [(x, content, lexEBNF (1, 0) content)]
  | otherwise =
    do
      content <- readFile x
      let front = (x, content, lexEBNF (1, 0) content)
      back <- lexFiles xs
      return (front:back)
