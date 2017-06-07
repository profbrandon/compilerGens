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

idHandler :: String -> String -> [Token]
idHandler [] [] = error "No identifier found"
idHandler [] name = [Token "Identifier" (reverse name)]
idHandler (x:xs) name 
	| isAlphaNum x || x == '_' || x == ' ' = idHandler xs (x:name)
	| otherwise = (Token "Identifier" (reverse name)):lexEBNF (x:xs)

lexEBNF :: String -> [Token]
lexEBNF [] = [Token "EOT" "$"]
lexEBNF (x:xs)
	| x == ' ' = lexEBNF xs
	| isAlphaNum x || x == '_' = idHandler xs [x]
    | elem x ops = 
        (Token t [v]):lexEBNF xs
    | otherwise = error "Incorrect EBNF Format"
    where (t, v) = head (filter (\k -> snd k == x) (zip ids ops))
