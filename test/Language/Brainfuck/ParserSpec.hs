module Language.Brainfuck.ParserSpec (parserSpec) where

import           Test.Hspec                (Spec, describe, it)
import           Test.Hspec.Parsec         (shouldFailOn, shouldParse)

import           Data.Text                 (Text)
import           Language.Brainfuck.Token  (BrainfuckToken (..))
import           Text.Parsec               (ParseError, Parsec, eof, parse)

import           Language.Brainfuck.Parser (brainfuckParser)

parserSpec :: Spec
parserSpec =
    describe "brainfuck parser" $ do
        it "should parse simple brainfuck codes" $
            let code = "++++----<[+-<>.]>++."
                expect =
                    [ IncrementToken, IncrementToken, IncrementToken, IncrementToken
                    , DecrementToken, DecrementToken, DecrementToken, DecrementToken
                    , PointerDecrementToken
                    , LoopToken
                        [ IncrementToken, DecrementToken, PointerDecrementToken, PointerIncrementToken
                        , OutputToken
                        ]
                    , PointerIncrementToken, IncrementToken, IncrementToken, OutputToken
                    ] in
                parseEof brainfuckParser code `shouldParse` expect

        it "should parse nested loops" $
            let code = "[++[--[>>]<<]..]"
                expect =
                    [ LoopToken
                        [ IncrementToken, IncrementToken
                        , LoopToken
                            [ DecrementToken, DecrementToken
                            , LoopToken [PointerIncrementToken, PointerIncrementToken]
                            , PointerDecrementToken, PointerDecrementToken
                            ]
                        , OutputToken, OutputToken
                        ]
                    ] in
                parseEof brainfuckParser code `shouldParse` expect

        it "should ignore unnecessary tokens" $
            let code = "A+AA+A[AAA+AA]AA+AAAA"
                expect =
                    [ IncrementToken, IncrementToken
                    , LoopToken [IncrementToken]
                    , IncrementToken
                    ] in
                parseEof brainfuckParser code `shouldParse` expect

        it "should not parse malformed loops" $
            parseEof brainfuckParser `shouldFailOn` "[[]"

parseEof :: Parsec Text () a -> Text -> Either ParseError a
parseEof parser = parse (parser <* eof) ""
