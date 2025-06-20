module Data.Text.Brainfuck (fromBrainfuck, fromBrainfuck') where

import           Data.Maybe         (fromJust)
import           Data.Text          (Text)
import           Language.Brainfuck (getOutput, runBrainfuck)

fromBrainfuck :: Text -> Maybe Text
fromBrainfuck = either (const Nothing) (Just . getOutput) . runBrainfuck

fromBrainfuck' :: Text -> Text
fromBrainfuck' = fromJust . fromBrainfuck
