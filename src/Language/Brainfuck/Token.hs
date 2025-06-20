module Language.Brainfuck.Token (BrainfuckToken (..)) where

data BrainfuckToken = IncrementToken
                    | DecrementToken
                    | PointerIncrementToken
                    | PointerDecrementToken
                    | LoopToken [BrainfuckToken]
                    | OutputToken
                    deriving Eq

instance Show BrainfuckToken where
    show IncrementToken        = "+"
    show DecrementToken        = "-"
    show PointerIncrementToken = ">"
    show PointerDecrementToken = "<"
    show (LoopToken tokens)    = "[" ++ concatMap show tokens ++ "]"
    show OutputToken           = "."
