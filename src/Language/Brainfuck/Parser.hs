module Language.Brainfuck.Parser (brainfuckParser) where

import           Control.Monad            (void)
import           Data.Functor             (($>))
import           Data.Text                (Text)
import           Language.Brainfuck.Token (BrainfuckToken (..))
import           Text.Parsec

brainfuckParser :: Parsec Text () [BrainfuckToken]
brainfuckParser = many $
    ignoreUntilValidToken *>
        ( try (char '+' $> IncrementToken)
      <|> try (char '-' $> DecrementToken)
      <|> try (char '>' $> PointerIncrementToken)
      <|> try (char '<' $> PointerDecrementToken)
      <|> try (char '[' *> (LoopToken <$> brainfuckParser) <* char ']')
      <|>     (char '.' $> OutputToken)
        )
    <* ignoreUntilValidToken

    where ignoreUntilValidToken = void $ manyTill anyChar (lookAhead (void (oneOf "+-[]<>.") <|> eof))
