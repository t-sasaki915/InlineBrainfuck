# InlineBrainfuck
A library for treating Brainfuck codes as Text or String

## Examples
```haskell
import Data.String.Brainfuck (fromBrainfuck')

main :: IO ()
main = putStrLn (fromBrainfuck' "-[------->+<]>-.---.+++++++..+++.") -- => HELLO
```
With compile-time syntax check:
```haskell
{-# LANGUAGE QuasiQuotes #-}

import Text.Brainfuck.QuasiQuote (bf)

main :: IO ()
main = putStrLn [bf|-[------->+<]>-.---.+++++++..+++.|] -- => HELLO
