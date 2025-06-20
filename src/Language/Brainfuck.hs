module Language.Brainfuck
    ( BrainfuckToken (..)
    , BrainfuckState (..)
    , ParseError
    , parseBrainfuck
    , evalBrainfuck
    , runBrainfuck
    ) where

import           Data.Text                 (Text)
import           Language.Brainfuck.Eval   (BrainfuckState (..), evalBrainfuck)
import           Language.Brainfuck.Parser (parseBrainfuck)
import           Language.Brainfuck.Token  (BrainfuckToken (..))
import           Text.Parsec               (ParseError)

runBrainfuck :: Text -> Either ParseError BrainfuckState
runBrainfuck = mapRight evalBrainfuck . parseBrainfuck
    where
        mapRight _ (Left x)  = Left x
        mapRight f (Right x) = Right (f x)
