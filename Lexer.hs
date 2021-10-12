module Lexer (Token (..), lexer) where

import Data.Char (isDigit)
import Data.List.Split (splitOn)

-- lexer
data Token
  = Num Double
  | Plus
  | Minus
  | Times
  | Divide
  | Pow
  | Floor
  | Ceil
  deriving (Show)

lexer :: String -> [Token]
lexer = lexer' . splitOn ","

lexer' :: [String] -> [Token]
lexer' [] = []
lexer' (x : xs)
  | x == "+" = Plus : lexer' xs
  | x == "-" = Minus : lexer' xs
  | x == "*" = Times : lexer' xs
  | x == "/" = Divide : lexer' xs
  | x == "**" = Pow : lexer' xs
  | x == "<" = Floor : lexer' xs
  | x == ">" = Ceil : lexer' xs
  | isNumeric x = Num (read x) : lexer' xs
  | otherwise = error "lexer': unknown operator/operand"

isNumeric :: String -> Bool
isNumeric str =
  case reads str :: [(Double, String)] of
    [(_, "")] -> True
    _ -> False