module Text.Brainfuck.QuasiQuote (bf) where

import           Data.String.Brainfuck     (fromBrainfuck')
import           Language.Haskell.TH.Lib   (litE, stringL)
import           Language.Haskell.TH.Quote (QuasiQuoter (..))

bf :: QuasiQuoter
bf = QuasiQuoter
    { quoteExp  = litE . stringL . fromBrainfuck'
    , quotePat  = undefined
    , quoteType = undefined
    , quoteDec  = undefined
    }
