{- Library for parsing ebnf formatted files -}

module EBNF where

import Data.Char

data Token = Token { tType::String
                   , tvalue::String
                   } deriving (Show)

idHandler :: (String, String) -> [Token]

ops   = ['=', ',', ';', '|', '[', ']', '{', '}', '(', ')', '\'', '\"', '*', '?', '-']
ids   = ["Definition", "Concatination", "Termination", "Alternation", "Left Optional"
        , "Right Optional", "Left Repetition", "Right Repetition", "Left Parenthesis"
        , "Right Parenthesis", "Single Quote Literal", "Double Quote Literal"
        , "Half Comment", "Special Sequence", "Exception"]

lexEBNF :: String -> [Token]
lexEBNF [] = (Token "EOT" "$"):[]
lexEBNF (x:xs) | isAlphaNum x = []
               | elem x ops = let left = ids !! (elemIndex x ops) in (uncurry (Token) (left, [x])):lexEBNF xs
               | otherwise = error "Incorrect EBNF Format"
               where z = zip ids ops
