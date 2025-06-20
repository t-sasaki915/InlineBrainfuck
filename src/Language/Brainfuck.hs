module Language.Brainfuck
    ( BrainfuckToken (..)
    , BrainfuckState
    , ParseError
    , getPointer
    , getMemory
    , getOutput
    , parseBrainfuck
    , evalBrainfuck
    , runBrainfuck
    ) where

import           Data.Text                 (Text)
import           Language.Brainfuck.Eval
import           Language.Brainfuck.Parser
import           Language.Brainfuck.Token
import           Text.Parsec               (ParseError)

runBrainfuck :: Text -> Either ParseError BrainfuckState
runBrainfuck = mapRight evalBrainfuck . parseBrainfuck
    where
        mapRight _ (Left x)  = Left x
        mapRight f (Right x) = Right (f x)
