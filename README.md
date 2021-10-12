# RPN_Calculator
a simple reverse polish notation calculator

## Build

Since the project is so simple,
don't waste your effort to compile it.
Just using `ghci` is enough.

## Run

We will use the cli environment `ghci` to interpret the code:

```sh
❯ ghci SimpleRPN.hs
GHCi, version 8.6.5: http://www.haskell.org/ghc/  :? for help
[1 of 2] Compiling Lexer            ( Lexer.hs, interpreted )
[2 of 2] Compiling SimpleRPN        ( SimpleRPN.hs, interpreted )
Ok, two modules loaded.
*SimpleRPN> rpn "2,5,+"
7.0
*SimpleRPN> rpn "2,12,6,-,/,5,3,+,*"
2.6666666666666665
*SimpleRPN>
```
