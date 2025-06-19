module Language.Brainfuck.Parser (parseBrainfuck) where

import           Control.Monad      (void)
import           Data.Functor       (($>))
import           Data.Text          (Text)
import           Language.Brainfuck (BrainfuckToken (..))
import           Text.Parsec

parseBrainfuck :: Parsec Text () [BrainfuckToken]
parseBrainfuck = many $
    ignoreUntilValidToken                                        *>
    try (char '+' $> IncrementToken)                             <|>
    try (char '-' $> DecrementToken)                             <|>
    try (char '>' $> PointerIncrementToken)                      <|>
    try (char '<' $> PointerDecrementToken)                      <|>
    try (char '[' *> (LoopToken <$> parseBrainfuck) <* char ']') <|>
        (char '.' $> OutputToken)                                <*
    ignoreUntilValidToken

    where ignoreUntilValidToken = void $ manyTill anyChar (oneOf "+-[]<>.")
