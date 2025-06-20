import           Test.Hspec                    (hspec)

import           Language.Brainfuck.ParserSpec (parserSpec)

main :: IO ()
main = hspec $ do
    parserSpec
