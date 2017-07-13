
module EBNF.Parser

where

import EBNF.Lexer

data Val = Val String
data Tag = Tag String

type Term = Either Val Tag

type Err = String

type Parser a = [Token] -> (Either a Err, [Token])

(>>=) :: Parser a -> (a -> Parser b) -> Parser b
pa >>= f =
  \ts -> let (da, ta) = pa ts
    in case da of
      Left a  -> f a ta 
      Right e -> (Right e, ts)

infixl 2 <|> 

(<|>) :: Parser a -> Parser b -> Parser (Either a b)
pa <|> pb =
  \ts ->
    let (da, ta) = pa ts
        (db, tb) = pb ts
    in case (da, db) of
      (Right ea, Right eb) -> (Right $ ea ++ " | " ++ eb, ts)
      (Left a  , Right eb) -> (Left (Left a), ta)
      (Right eb, Left b)   -> (Left (Right b), ta)
      _ -> (Right "ambiguous case in parser <|>", ts)

literal :: Parser Val
literal []             = (Nothing, [e], []) where e = Err "end of tokens, but literal expected" (Info 0 0) (Info 0 0)
literal ((Lit s i):ts) = (Just $ Val s, [], ts)
literal ts             = (Nothing, [e], ts) where e = Err "literal token expected" (Info 0 0) (Info 0 0)

identifier :: Parser Tag
identifier []            = (Nothing, [e], []) where e = Err "end of tokens, but identifier expected" (Info 0 0) (Info 0 0)
identifier ((Id s i):ts) = (Just $ Val s, [], ts)
identifier ts            = (Nothing, [e], ts) where e = Err "identifier token expected" (Info 0 0) (Info 0 0)

term :: Parser Term
term = identifier <|> literal
