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
          out <- lexFiles args
          let toks = fst out
          let errs = snd out
          mapM (putToken) toks
          putStrLn "--------------------"
          mapM (putError) errs
          putStrLn "--------------------"
      else error "Not all files exist as given"

lexFiles :: [String] -> IO ([Token],[Error])
lexFiles [] = error "No files provided"
lexFiles (x:xs)
  | xs == [] =
    do
      content <- readFile x
      return (lexEBNF content)
  | otherwise =
    do
      content <- readFile x
      let front = lexEBNF content
      back <- lexFiles xs
      return (fst front ++ fst back, snd front ++ snd back)
