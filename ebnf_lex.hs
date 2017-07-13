-- Library for parsing ebnf formatted files

module EBNF.Lexer
  ( Info(..)
  , Token(..)
  , lexEBNF
  , showTokens
  , showToken
  , showError
  )
where

import Data.Char
import Data.List
import Prelude hiding (until)



-- Datatype Declarations

data Info = Info { line   :: Int
                 , column :: Int }

data Token = Def  Info
           | Con  Info
           | Alt  Info
           | EDef Info
           | LOpt Info
           | ROpt Info
           | LRep Info
           | RRep Info
           | LPar Info
           | RPar Info
           | Ex   Info
           | Id   String Info
           | Lit  String Info
           | Sp   String Info
           | Err  String Info Info
           | EOF



-- Show Functions

instance Show Info where
  show (Info l c) = "line: " ++ show l ++ ", column: " ++ show c

instance Show Token where
  show = showToken

showTokens :: [Token] -> String
showTokens = unlines . (map (show))

tokFormat :: Info -> String -> String
tokFormat i s = "<" ++ show i ++ ", " ++ s ++ ">"

showToken :: Token -> String
showToken (Def   i)     = tokFormat i "Definition"
showToken (Con   i)     = tokFormat i "Concatination"
showToken (Alt   i)     = tokFormat i "Alternation"
showToken (EDef  i)     = tokFormat i "End of Definition"
showToken (LOpt  i)     = tokFormat i "Left Optional"
showToken (ROpt  i)     = tokFormat i "Right Optional"
showToken (LRep  i)     = tokFormat i "Left Repetition"
showToken (RRep  i)     = tokFormat i "Right Repetition"
showToken (LPar  i)     = tokFormat i "Left Parenthesis"
showToken (RPar  i)     = tokFormat i "Right Parenthesis"
showToken (Ex    i)     = tokFormat i "Exception"
showToken (Id  s i)     = tokFormat i $ "Identifier: \'" ++ s ++ "\'"
showToken (Lit s i)     = tokFormat i $ "Literal: \'" ++ s ++ "\'"
showToken (Sp  s i)     = tokFormat i $ "Special Sequence: \'" ++ s ++ "\'"
showToken (Err s i1 i2) = "<error: \'" ++ s ++ "\'>"
showToken (EOF)         = "<EOF>"

showError (Err s i1 i2) file =
  let Info ls _ = i1
      Info le _ = i2
      back       = getLnsStr ls le file
  in case back of
    Nothing -> error "Error indices incorrect"
    Just b  -> "Error in string \'" ++ s ++ "\':\n" ++ b

getLnsStr :: Int -> Int -> String -> Maybe String
getLnsStr ls le s
  | 0 <= ls && ls <= le && le < length s' =
    Just $ unlines $ filter (between ls le) s'
  | otherwise = Nothing
  where s'            = lines s
        between ls le = \l -> case elemIndex l s' of
                                Nothing -> False
                                Just i  -> ls <= i && i <= le


-- Lexing

lexEBNF :: (Int,Int) -> String -> [Token]
lexEBNF (l,c) []        = [EOF]
lexEBNF (l,c) ('\n':xs) = lexEBNF (l + 1, 0) xs
lexEBNF (l,c) (x:xs)
  | isSpace x = back
  | isAlpha x =
    let (p, name, xs') = identifier (l, c + 1) xs
    in (Id (x:name) $ Info l c):lexEBNF p xs'
  | otherwise =
    case x of
      '='  -> tok Def
      ','  -> tok Con
      '|'  -> tok Alt
      ';'  -> tok EDef
      '['  -> tok LOpt
      ']'  -> tok ROpt
      '{'  -> tok LRep
      '}'  -> tok RRep
      ')'  -> tok RPar
      '-'  -> tok Ex
      '\'' -> txt Lit $ "\'"
      '\"' -> txt Lit $ "\""
      '?'  -> txt Sp $ "?"
      '('  ->
        case xs of
          '*':xs' -> com xs'
          _       -> tok LPar
      _    -> err
  where back = lexEBNF (l, c + 1) xs
        tok  = \x   -> (x $ Info l c):back
        txt  = \x y -> let (p, text, xs') = until (l, c + 1) y xs
                       in (x text $ Info l c):lexEBNF p xs'
        com  = \bck -> let (p, text, xs') = until (l, c + 1) "*)" bck
                       in lexEBNF p xs'
        err  = let (t:ts) = back
               in case t of
                 Err s i1 i2 -> (Err (x:s) (Info l c) i2):ts
                 _           -> (Err [x] (Info l c) (Info l c)):back



-- Auxiliary Lexing Functions

identifier :: (Int,Int) -> String -> ((Int,Int),String,String)
identifier (l, c) ""     = ((l, c), "", "")
identifier (l, c) (x:xs)
  | isAlphaNum x = (p, x:name, xs')
  | x == ' '     =
    if null name then (p, name, xs') else (p, x:name, xs')
  | otherwise    = ((l, c), "", (x:xs))
  where (p, name, xs') = identifier (l, c + 1) xs

until :: (Int,Int) -> String -> String -> ((Int,Int),String,String)
until (l, c) str "" = error $ str ++ " expected"
until (l, c) str back
  | str `isPrefixOf` back =
    let back' = drop (length str) back
    in ((l, c + length str), "", back')
  | otherwise = (p, x:text, xs')
    where (x:xs)         = back
          (p, text, xs') = until (l, c + 1) str xs
