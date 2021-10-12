module SimpleRPN where

import Foreign.C (CIntMax (CIntMax))
import Lexer (Token (..), lexer)

-- data
data Expr
  = ENum Double
  | EAdd Expr Expr
  | EMult Expr Expr
  | ESubtr Expr Expr
  | EDiv Expr Expr
  | EPow Expr Expr
  | EFloor Expr
  | ECeil Expr
  | ENull
  deriving (Show)

-- produce the complete parse tree
parse :: [Token] -> (Expr, [Token])
parse [] = (ENull, [])
parse (x : xs) = case x of
  Num a -> (ENum a, xs)
  Plus -> (EAdd expr1 expr2, rest2)
  Minus -> (ESubtr expr1 expr2, rest2)
  Times -> (EMult expr1 expr2, rest2)
  Divide -> (EDiv expr1 expr2, rest2)
  Pow -> (EPow expr1 expr2, rest2)
  Floor -> (EFloor expr2, rest1)
  Ceil -> (ECeil expr2, rest1)
  where
    (expr2, rest1) = parse xs
    (expr1, rest2) = parse rest1

calculate :: Expr -> Double
calculate ENull = 0
calculate (ENum a) = a
calculate (EAdd expr1 expr2) = (+) (calculate expr1) (calculate expr2)
calculate (EMult expr1 expr2) = (*) (calculate expr1) (calculate expr2)
calculate (ESubtr expr1 expr2) = (-) (calculate expr1) (calculate expr2)
calculate (EDiv expr1 expr2) = (/) (calculate expr1) (calculate expr2)
calculate (EPow expr1 expr2) = (**) (calculate expr1) (calculate expr2)
calculate (EFloor expr) = (fromIntegral . floor . calculate) expr
calculate (ECeil expr) = (fromIntegral . ceiling . calculate) expr

rpn :: String -> Double
rpn = calculate . fst . parse . reverse . lexer
