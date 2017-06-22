{- Library for parsing ebnf formatted files -}

module EBNF
  ( Token(..)
  , putTok
  , lexEBNF)
where

import Data.Char

data Token = Token { tType::String
                   , tValue::String
                   } deriving (Show)

putTok :: Token -> IO ()
putTok (Token t v) = putStr ("<type=" ++ t ++ ", value=" ++ v ++ ">\n")

ops   = ['=', ',', ';', '|', '[', ']', '{', '}', '(', ')', '\'', '\"', '*', '?', '-']
ids   = ["Definition", "Concatination", "Termination", "Alternation", "Left Optional"
        , "Right Optional", "Left Repetition", "Right Repetition", "Left Parenthesis"
        , "Right Parenthesis", "Single Quote Literal", "Double Quote Literal"
        , "Half Comment", "Special Sequence", "Exception"]

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

idHandler :: String -> String -> [Token]
idHandler = sHandler (\c s -> isAlphaNum c || c == '_' || c == ' ') True "Identifier"

spHandler :: String -> String -> [Token]
spHandler = sHandler (notChar '?') False "Special Sequence"

sqHandler :: String -> String -> [Token]
sqHandler = sHandler (notChar '\'') False "Single Quote Literal"

dqHandler :: String -> String -> [Token]
dqHandler = sHandler (notChar '\"') False "Double Quote Literal"

{- sHandler handles sequences of characters.  The first argument is a function that
   determines if a character should be appended to the sequence.  The second argument
   determines whether the last character needs to be parsed.  The third is a string
   representing the token type -}
sHandler :: (Char -> String -> Bool) -> Bool -> String -> String -> String -> [Token]
sHandler _ _ [] _ _ = error "No type supplied to token"
sHandler _ _ t [] [] = error ("No " ++ t ++ " found")
sHandler _ _ t [] seq = [Token t (reverse (trimSpace seq))]
sHandler cond append t (x:xs) seq
  | cond x (reverse seq) = sHandler cond append t xs (x:seq)
  | otherwise = (Token t (reverse (trimSpace seq))):tokenFinder (if append then x:xs else xs)

cHandler :: String -> String -> [Token]
cHandler = sHandler (\c s -> ')' /= c || last s /= '*') False "Comment"

tokenFinder :: String -> [Token]
tokenFinder [] = [Token "EOT" "$"]
tokenFinder (x:xs)
  | isSpace x = tokenFinder xs
  | x == '?' = spHandler xs []
  | x == '\'' = sqHandler xs []
  | x == '\"' = dqHandler xs []
  | x == '(' && head xs == '*' = cHandler (tail xs) []
  | isAlpha x || x == '_' = idHandler xs [x]
  | elem x ops = 
        (Token t [v]):tokenFinder xs
  | otherwise = error "Incorrect EBNF Format"
  where (t, v) = head (filter (\k -> snd k == x) (zip ids ops))

lexEBNF :: String -> [Token]
lexEBNF s = 
  filter (\tok -> let Token t v = tok in t /= "Comment") (tokenFinder s)