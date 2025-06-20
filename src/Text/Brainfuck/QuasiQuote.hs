module Text.Brainfuck.QuasiQuote (bf) where

import           Language.Haskell.TH.Lib   (litE, stringL)
import           Language.Haskell.TH.Quote (QuasiQuoter (..))
import           Text.Brainfuck            (fromBrainfuck')

bf :: QuasiQuoter
bf = QuasiQuoter
    { quoteExp  = litE . stringL . fromBrainfuck'
    , quotePat  = undefined
    , quoteType = undefined
    , quoteDec  = undefined
    }
