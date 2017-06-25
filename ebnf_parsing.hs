-- Library for parsing ebnf formatted files

module EBNF
  ( Output(..)
  , putOutput
  , trimSpace
  , lexEBNF)
where

import Data.Char

data Output = Token { tType  :: String
                    , tValue :: String
                    , tLine  :: Int }
            | Error { eValue :: String
                    , eLine  :: Int } deriving (Show)

putOutput :: Output -> IO ()
putOutput (Token t v l) = putStr ("<type=" ++ t ++ ", value=" ++ v ++ ", line=" ++ show l ++ ">\n")
putOutput (Error v l) = putStr ("Error in:\n" ++ show v ++ "\nat line " ++ show l ++ "\n")

ops   = ['=', ',', ';', '|', '[', ']', '{', '}', '(', ')', '*', '?', '-']
ids   = [ "Definition", "Concatination", "Termination", "Alternation", "Left Optional"
        , "Right Optional", "Left Repetition", "Right Repetition", "Left Parenthesis"
        , "Right Parenthesis", "Half Comment", "Special Sequence", "Exception"]

remfrntSpace :: String -> String --Jason did this :D
remfrntSpace [] = []
remfrntSpace (x:xs) 
  | isSpace x = remfrntSpace xs
  | otherwise = x:xs

remBackSpace :: String -> String
remBackSpace = reverse . remfrntSpace . reverse

-- trimSpace removes whitespace from the front and back of text
trimSpace :: String -> String
trimSpace = remfrntSpace . remBackSpace

-- notChar determines if two characters are not equivalent (escape characters produce True)
notChar :: Char -> Char -> String -> Bool
notChar c1 c2 [] = c1 /= c2
notChar c1 c2 l = c1 /= c2 || (last l) == '\\'

idHandler :: Int -> String -> String -> [Output]
idHandler = sHandler (\c s -> isAlphaNum c || c == '_' || c == ' ') True "Identifier"

spHandler :: Int -> String -> String -> [Output]
spHandler = sHandler (notChar '?') False "Special Sequence"

sqHandler :: Int -> String -> String -> [Output]
sqHandler = sHandler (notChar '\'') False "Literal"

dqHandler :: Int -> String -> String -> [Output]
dqHandler = sHandler (notChar '\"') False "Literal"

{- sHandler handles sequences of characters.  The first argument is a function that
   determines if a character should be appended to the sequence.  The second argument
   determines whether the last character needs to be parsed.  The third is a string
   representing the token type -}
sHandler :: (Char -> String -> Bool) -> Bool -> String -> Int ->  String -> String -> [Output]
sHandler _ _ [] _ _ _ = error "No type supplied to token"
sHandler _ _ t _ [] [] = error ("No " ++ t ++ " found")
sHandler _ _ t ln [] seq = [Token t (reverse (trimSpace seq)) ln]
sHandler cond append t ln (x:xs) seq
  | x == '\n' = sHandler cond append t (ln + 1) xs seq
  | cond x (reverse seq) = sHandler cond append t ln xs (x:seq)
  | otherwise = (Token t (reverse (trimSpace seq)) ln):tokenFinder ln (if append then x:xs else xs)

cHandler :: Int -> String -> String -> [Output]
cHandler = sHandler (\c s -> ')' /= c || last s /= '*') False "Comment"

tokenFinder :: Int -> String -> [Output]
tokenFinder ln [] = [Token "EOT" "$" ln]
tokenFinder ln (x:xs)
  | x == '\n' = tokenFinder (ln + 1) xs
  | isSpace x = tokenFinder ln xs
  | x == '?' = spHandler ln xs []
  | x == '\'' = sqHandler ln xs []
  | x == '\"' = dqHandler ln xs []
  | x == '(' && head xs == '*' = cHandler ln (tail xs) []
  | isAlpha x || x == '_' = idHandler ln xs [x]
  | elem x ops = 
    (Token t [v] ln):tokenFinder ln xs
  | otherwise =
    let (y:ys) = tokenFinder ln xs
    in case y of
      Error val l -> (Error (x:val) ln):ys
      Token _ _ _ -> (Error [x] ln):y:ys
  where (t, v) = head (filter (\k -> snd k == x) (zip ids ops))

lexEBNF :: String -> [Output]
lexEBNF s =
  let isComment = \out -> case out of
                            Token "Comment" _ _ -> True
                            _ -> False
  in filter (not isComment) (tokenFinder 1 s)
