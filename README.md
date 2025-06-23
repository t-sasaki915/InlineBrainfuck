# InlineBrainfuck
Brainfuck QuasiQuoter Library

## Examples
```haskell
import Text.Brainfuck (fromBrainfuck')

main :: IO ()
main = putStrLn (fromBrainfuck' "-[------->+<]>-.---.+++++++..+++.") -- => HELLO
```
With compile-time syntax check:
```haskell
{-# LANGUAGE QuasiQuotes #-}

import Text.Brainfuck.QuasiQuote (bf)

main :: IO ()
main = putStrLn [bf|-[------->+<]>-.---.+++++++..+++.|] -- => HELLO
