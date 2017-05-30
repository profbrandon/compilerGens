{- Library for parsing ebnf formatted files -}

module EBNF where

import Data.Char

data Token = Token { tType::String
                   , tvalue::String
                   } deriving (Show)
                   
lexEBNF :: String -> [Token]
lexEBNF []     = (Token "EOT" "$"):[]
lexEBNF (x:xs) | isAlphaNum x = []
lexEBNF (x:xs) = case x of
                    ' ' -> lexEBNF xs
                    '=' -> back "DEF" "="
                    ',' -> back "CON" ","
                    ';' -> back "TER" ";"
                    '|' -> back "ALT" "|"
                    '[' -> back "LSB" "["
                    ']' -> back "RSB" "]"
                    '{' -> back "LCB" "{"
                    '}' -> back "RCB" "}"
                    '(' -> back "LP" "("
                    ')' -> back "RP" ")"
                    '\"' -> back "DQ" "\""
                    '\'' -> back "SQ" "\'"
                    '*' -> back "COM" "*"
                    '?' -> back "SSE" "?"
                    '-' -> back "EXC" "-"
                    _ -> error "incorrect grammar format"
                    where back = \x y -> (Token x y):(lexEBNF xs)

