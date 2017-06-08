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

remSpace :: String -> String
remSpace = remfrntSpace . remBackSpace

idHandler :: String -> String -> [Token]
idHandler [] [] = error "No identifier found"
idHandler [] name = [Token "Identifier" (reverse (remSpace name))]
idHandler (x:xs) name 
	| isAlphaNum x || x == '_' || x == ' ' = idHandler xs (x:name)
	| otherwise = (Token "Identifier" (reverse (remSpace name))):lexEBNF (x:xs)

lexEBNF :: String -> [Token]
lexEBNF [] = [Token "EOT" "$"]
lexEBNF (x:xs)
	| x == ' ' = lexEBNF xs
	| isAlpha x || x == '_' = idHandler xs [x]
    | elem x ops = 
        (Token t [v]):lexEBNF xs
    | otherwise = error "Incorrect EBNF Format"
    where (t, v) = head (filter (\k -> snd k == x) (zip ids ops))
