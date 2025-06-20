module Language.Brainfuck.EvalSpec (evalSpec) where

import           Test.Hspec              (Spec, describe, it, shouldBe)

import           Data.Text               (Text)
import qualified Data.Text               as Text
import           Language.Brainfuck      (parseBrainfuck)

import           Language.Brainfuck.Eval (evalBrainfuck, getOutput)

evalSpec :: Spec
evalSpec =
    describe "brainfuck evaluator" $ do
        it "should evaluate brainfuck code 1" $
            let code = "--------[-->+++<]>.+++[->+++<]>.[--->+<]>----.+."
                expect = "test" in
                    evalBf code `shouldBe` expect

        it "should evaluate brainfuck code 2" $
            let code = "-[------->+<]>-.---.+++++++..+++."
                expect = "HELLO" in
                    evalBf code `shouldBe` expect

        it "should evaluate brainfuck code 3" $
            let code = "-[----->+<]>--.+.+.+.+.+.+.+.+.---------."
                expect = "1234567890" in
                    evalBf code `shouldBe` expect

        it "should evaluate brainfuck code 4" $
            let code = "-[------->+<]>.+++++.--.---.+++++.---------.-[-->+<]>--.+[->++<]>.>-[--->+<]>---.--[->++++<]>+.++++++++.+++++.--------.>-[--->+<]>.-[----->+<]>-.++++++++."
                expect = "INLINE BRAINFUCK" in
                    evalBf code `shouldBe` expect

        it "should evaluate brainfuck code 5" $
            let code = "-[------->+<]>+++.[------>+<]>-.+++++++++++++.-------.--[--->+<]>--.[->+++<]>++.++++++.--.[->+++<]>-.--[-->+++<]>.---[----->+<]>-.+++[->+++<]>++.++++++++.+++++.--------.-[--->+<]>--.+[->+++<]>+.++++++++.[->++++++++++<]>.[-->+++<]>.-[----->+<]>++.[->+++<]>-.+++++++++++."
                expect = "Language.Brainfuck.Eval" in
                    evalBf code `shouldBe` expect

evalBf :: Text -> Text
evalBf = either Text.show (getOutput . evalBrainfuck) . parseBrainfuck
