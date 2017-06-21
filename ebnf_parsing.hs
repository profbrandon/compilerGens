{- Library for parsing ebnf formatted files -}

module EBNF where

import Data.Char

data Token = Token { tType::String
                   , tvalue::String
                   } deriving (Show)

ops   = ['=', ',', ';', '|', '[', ']', '{', '}', '(', ')', '\'', '\"', '*', '?', '-']
ids   = ["Definition", "Concatination", "Termination", "Alternation", "Left Optional"
        , "Right Optional", "Left Repetition", "Right Repetition", "Left Parenthesis"
        , "Right Parenthesis", "Single Quote Literal", "Double Quote Literal"
        , "Half Comment", "Special Sequence", "Exception"]


remfrntSpace :: String -> String --I did this :D
remfrntSpace [] = []
remfrntSpace (x:xs) 
  | x == ' ' = remfrntSpace xs
  | otherwise = x:xs

remBackSpace :: String -> String
remBackSpace = reverse . remfrntSpace . reverse

trimSpace :: String -> String
trimSpace = remfrntSpace . remBackSpace

idHandler :: String -> String -> [Token]
idHandler = sHandler (\c -> isAlphaNum c || c == '_' || c == ' ') "Identifier"

spHandler :: String -> String -> [Token]
spHandler = sHandler (/= '?') "Special Sequence"

sqHandler :: String -> String -> [Token]

sqHandler = sHandler (/= '\'') "Single Quote Literal"

dqHandler :: String -> String -> [Token]
dqHandler = sHandler (/= '\"') "Double Quote Literal"

sHandler :: (Char -> Bool) -> String -> String -> String -> [Token]
sHandler _ [] _ _ = error "No type supplied to token"
sHandler _ t [] [] = error ("No " ++ t ++ " found")
sHandler _ t [] seq = [Token t (reverse (trimSpace seq))]
sHandler cond t (x:xs) seq
  | cond x = sHandler cond t xs (x:seq)
  | otherwise = (Token t (reverse (trimSpace seq))):lexEBNF (x:xs)

lexEBNF :: String -> [Token]
lexEBNF [] = [Token "EOT" "$"]
lexEBNF (x:xs)
  | x == ' ' = lexEBNF xs
  | x == '?' = spHandler xs []
  | x == '\'' = sqHandler xs []
  | x == '\"' = dqHandler xs []
  | isAlpha x || x == '_' = idHandler xs [x]
  | elem x ops = 
        (Token t [v]):lexEBNF xs
  | otherwise = error "Incorrect EBNF Format"
  where (t, v) = head (filter (\k -> snd k == x) (zip ids ops))
