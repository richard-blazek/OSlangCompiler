module Lexer (Token (..), tokenise) where

import Data.Char (ord, chr)

data Token
  = Number Integer
  | Text String
  | Operator String
  | Word String deriving (Show, Eq, Ord)

isDigit c = c >= '0' && c <= '9'
isHexDigit c = isDigit c || (c >= 'A' && c <= 'F')
isAlpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'
isAlnum c = isAlpha c || isDigit c
isOperator = (`elem` "()[]{}.,:$+-*/%&|~^<>=!")
isCompound = (`elem` ["<-", "==", "!=", "<", ">", ">=", "<="])
isSpace = (`elem` " \t\n\r\f\v")

digit :: Char -> Int
digit c = if isDigit c then ord c - ord '0' else ord c - ord 'A' + 10

skipComment :: String -> String
skipComment ('\n' : code) = code
skipComment (_ : code) = skipComment code

parseNumber :: Integer -> Integer -> String -> (Token, String)
parseNumber 10 n (c : code) | isDigit c = parseNumber 10 (10 * n + toInteger (digit c)) code
parseNumber 16 n (c : code) | isHexDigit c = parseNumber 16 (16 * n + toInteger (digit c)) code
parseNumber _ n code = (Number n, code)

parseText :: String -> String -> (Token, String)
parseText t ('"' : code) = (Text (reverse t), code)
parseText t ('\\' : d1 : d2 : code) = parseText (chr (digit d1 * 16 + digit d2) : t) code
parseText t (c : code) = parseText (c : t) code

parseOperator :: Char -> String -> (Token, String)
parseOperator o1 (o2 : code) | isCompound [o1, o2] = (Operator [o1, o2], code)
parseOperator o1 code = (Operator [o1], code)

parseWord :: String -> String -> (Token, String)
parseWord w (c : code) | isAlnum c = parseWord (c : w) code
parseWord w code = (Word (reverse w), code)

nextToken :: [Token] -> String -> [Token]
nextToken tokens ('#' : rest) = nextToken tokens (skipComment rest)
nextToken tokens ('0' : 'x' : rest) = let (token, code) = parseNumber 16 0 rest in nextToken (token : tokens) code
nextToken tokens (c : rest) | isDigit c = let (token, code) = parseNumber 10 0 rest in nextToken (token : tokens) code
nextToken tokens ('"' : rest) = let (token, code) = parseText "" rest in nextToken (token : tokens) code
nextToken tokens (c : rest) | isOperator c = let (token, code) = parseOperator c rest in nextToken (token : tokens) code
nextToken tokens (c : rest) | isAlpha c = let (token, code) = parseWord "" rest in nextToken (token : tokens) code
nextToken tokens (c : rest) | isSpace c = nextToken tokens rest
nextToken tokens "" = reverse tokens

tokenise :: String -> [Token]
tokenise = nextToken []
