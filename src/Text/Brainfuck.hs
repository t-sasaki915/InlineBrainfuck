module Text.Brainfuck (fromBrainfuck, fromBrainfuck') where

import           Data.Text.Conversions (FromText (..), ToText (..))
import           GHC.Stack             (HasCallStack)
import           Language.Brainfuck    (getOutput, runBrainfuck)

fromBrainfuck :: (FromText a, ToText a) => a -> Maybe a
fromBrainfuck = either (const Nothing) (Just . fromText . getOutput) . runBrainfuck . toText

fromBrainfuck' :: (HasCallStack, FromText a, ToText a) => a -> a
fromBrainfuck' = either (error . show) (fromText . getOutput) . runBrainfuck . toText
