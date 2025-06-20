module Data.String.Brainfuck (fromBrainfuck, fromBrainfuck') where

import           Data.Text           (pack, unpack)
import qualified Data.Text.Brainfuck as TextBrainfuck

fromBrainfuck :: String -> Maybe String
fromBrainfuck = (unpack <$>) . TextBrainfuck.fromBrainfuck . pack

fromBrainfuck' :: String -> String
fromBrainfuck' = unpack . TextBrainfuck.fromBrainfuck' . pack
