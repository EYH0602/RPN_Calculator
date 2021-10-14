module SimpleRPN (Expr (..), parse, calculate, rpn, getExpr) where

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
calculate (EFloor expr) = (fromIntegral . floor . calculate) expr
calculate (ECeil expr) = (fromIntegral . ceiling . calculate) expr
calculate (EAdd expr1 expr2) = calculate expr1 + calculate expr2
calculate (EMult expr1 expr2) = calculate expr1 * calculate expr2
calculate (ESubtr expr1 expr2) = calculate expr1 - calculate expr2
calculate (EDiv expr1 expr2) = calculate expr1 / calculate expr2
calculate (EPow expr1 expr2) = calculate expr1 ** calculate expr2

rpn :: String -> Double
rpn = calculate . getExpr

getExpr :: String -> Expr
getExpr = fst . parse . reverse . lexer

getExprStr :: Expr -> String
getExprStr expr = case expr of
  ENum x -> show x
  EAdd ex ex' -> "+"
  EMult ex ex' -> "*"
  ESubtr ex ex' -> "-"
  EDiv ex ex' -> "/"
  EPow ex ex' -> "**"
  EFloor ex -> "<"
  ECeil ex -> ">"
  ENull -> ""

instance Show Expr where
  show = expr2str 0

indent :: Int -> String
indent 0 = ""
indent n = "  " ++ indent (n - 1)

expr2str :: Int -> Expr -> String
expr2str _ ENull = ""
expr2str n (ENum x) = indent n ++ show x ++ "\n"
expr2str n (EAdd ex ex') = indent n ++ "(+\n" ++ expr2str (n + 1) ex ++ expr2str (n + 1) ex' ++ indent n ++ ")\n"
expr2str n (EMult ex ex') = indent n ++ "(*\n" ++ expr2str (n + 1) ex ++ expr2str (n + 1) ex' ++ indent n ++ ")\n"
expr2str n (ESubtr ex ex') = indent n ++ "(-\n" ++ expr2str (n + 1) ex ++ expr2str (n + 1) ex' ++ indent n ++ ")\n"
expr2str n (EDiv ex ex') = indent n ++ "(/\n" ++ expr2str (n + 1) ex ++ expr2str (n + 1) ex' ++ indent n ++ ")\n"
expr2str n (EPow ex ex') = indent n ++ "(**\n" ++ expr2str (n + 1) ex ++ expr2str (n + 1) ex' ++ indent n ++ ")\n"
expr2str n (EFloor ex) = indent n ++ "(<\n" ++ expr2str (n + 1) ex ++ indent n ++ ")\n"
expr2str n (ECeil ex) = indent n ++ "(>\n" ++ expr2str (n + 1) ex ++ indent n ++ ")\n"
