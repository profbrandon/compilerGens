// Library for parsing ebnf formatted files

import Data.Char

module EBNF where

data Token = Token { tType::String
                   , tvalue::String
                   }
                   
lexEBNF :: String -> [Token]
lexEBNF "" = (Token "EOT" "$"):[]
lexEBNF (x:xs) | x == ' ' = lexEBNF xs
               | x == '|' = (Token "Alt" "|"):lexEBNF xs
               | otherwise = []
