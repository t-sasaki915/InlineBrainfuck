module Language.Brainfuck.Parser
    ( BrainfuckToken (..)
    , parseBrainfuck
    , incrementToken
    , decrementToken
    , pointerIncrementToken
    , pointerDecrementToken
    , loopToken
    , outputToken
    ) where

import           Control.Monad      (void)
import           Data.Functor       (($>))
import           Data.Text          (Text)
import           Language.Brainfuck (BrainfuckToken (..))
import           Text.Parsec

parseBrainfuck :: Parsec Text () [BrainfuckToken]
parseBrainfuck = many $
    ignoreUntilValidToken *>
    try incrementToken <|>
    try decrementToken <|>
    try pointerIncrementToken <|>
    try pointerDecrementToken <|>
    try loopToken <|>
    outputToken <*
    ignoreUntilValidToken

incrementToken :: Parsec Text () BrainfuckToken
incrementToken = char '+' $> IncrementToken

decrementToken :: Parsec Text () BrainfuckToken
decrementToken = char '-' $> DecrementToken

pointerIncrementToken :: Parsec Text () BrainfuckToken
pointerIncrementToken = char '>' $> PointerIncrementToken

pointerDecrementToken :: Parsec Text () BrainfuckToken
pointerDecrementToken = char '<' $> PointerDecrementToken

loopToken :: Parsec Text () BrainfuckToken
loopToken = char '[' *> (LoopToken <$> parseBrainfuck) <* char ']'

outputToken :: Parsec Text () BrainfuckToken
outputToken = char '.' $> OutputToken

ignoreUntilValidToken :: Parsec Text () ()
ignoreUntilValidToken = void $ manyTill anyChar (oneOf "+-[]<>.")
