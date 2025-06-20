import           Test.Hspec                    (hspec)

import           Language.Brainfuck.EvalSpec   (evalSpec)
import           Language.Brainfuck.ParserSpec (parserSpec)

main :: IO ()
main = hspec $ do
    parserSpec
    evalSpec
