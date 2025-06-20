module Language.Brainfuck.Eval
    ( BrainfuckState
    , getPointer
    , getMemory
    , getOutput
    , evalBrainfuck
    , evalBrainfuckTokens
    ) where

import           Control.Monad                    (unless)
import           Control.Monad.Loops              (whileM_)
import           Control.Monad.Trans.State.Strict (State, execState)
import           Data.Char                        (chr)
import           Data.Functor                     ((<&>))
import           Language.Brainfuck.Eval.Internal
import           Language.Brainfuck.Token         (BrainfuckToken (..))

evalBrainfuck :: [BrainfuckToken] -> BrainfuckState
evalBrainfuck tokens = execState (evalBrainfuckTokens tokens) initialBrainfuckState

evalBrainfuckTokens :: [BrainfuckToken] -> State BrainfuckState ()
evalBrainfuckTokens = mapM_ $ \case
    IncrementToken ->
        currentValue >>= \case
            255 -> setCurrentValue 0
            n   -> setCurrentValue (n + 1)

    DecrementToken ->
        currentValue >>= \case
            0 -> setCurrentValue 255
            n -> setCurrentValue (n - 1)

    PointerIncrementToken ->
        currentPointer >>=
            setPointer . (+ 1)

    PointerDecrementToken ->
        currentPointer >>= \pointer ->
            unless (pointer == 0) $
                setPointer (pointer - 1)

    LoopToken loop ->
        whileM_ (currentValue <&> (/= 0)) $
            evalBrainfuckTokens loop

    OutputToken ->
        currentValue >>=
            appendToOutput . chr
