import System.Environment
import System.Directory
import EBNF

main :: IO ()
main =
  do
    args <- getArgs
    bools <- mapM (doesFileExist) args
    let exists = and (bools)
    if exists
      then
        do
          toks <- lexFiles args
          mapM (putOutput) toks
          putStr ""
      else error "Not all files exist as given"

lexFiles :: [String] -> IO [Output]
lexFiles [] = error "No files provided"
lexFiles (x:xs)
  | xs == [] =
    do
      content <- readFile x
      return (lexEBNF content)
  | otherwise =
    do
      content <- readFile x
      back <- lexFiles xs
      return (lexEBNF content ++ back)
