{-# LANGUAGE TemplateHaskell #-}

module Language.Brainfuck.Eval.Internal
    ( BrainfuckState
    , getPointer
    , getMemory
    , getOutput
    , initialBrainfuckState
    , currentPointer
    , setPointer
    , currentValue
    , setCurrentValue
    , appendToOutput
    ) where

import           Control.Lens                     (makeLenses, over, set, (^.))
import           Control.Monad.Trans.State.Strict (State, get, put)
import           Data.Map.Strict                  (Map)
import qualified Data.Map.Strict                  as Map
import           Data.Text                        (Text)
import qualified Data.Text                        as Text

data BrainfuckState = BrainfuckState
    { _pointer :: Int
    , _memory  :: Map Int Int
    , _output  :: Text
    }

initialBrainfuckState :: BrainfuckState
initialBrainfuckState =
    BrainfuckState
        { _pointer = 0
        , _memory  = Map.empty
        , _output  = ""
        }

getPointer :: BrainfuckState -> Int
getPointer = _pointer

getMemory :: BrainfuckState -> Map Int Int
getMemory = _memory

getOutput :: BrainfuckState -> Text
getOutput = _output

makeLenses ''BrainfuckState

currentPointer :: State BrainfuckState Int
currentPointer = get >>= \state -> pure (state ^. pointer)

setPointer :: Int -> State BrainfuckState ()
setPointer newPointer = get >>= put . set pointer newPointer

currentValue :: State BrainfuckState Int
currentValue =
    currentPointer >>= \pointer' ->
        currentMemory >>= \memory' ->
            case Map.lookup pointer' memory' of
                Just value -> pure value
                Nothing    -> pure 0

setCurrentValue :: Int -> State BrainfuckState ()
setCurrentValue newValue =
    currentPointer >>= \pointer' ->
        currentMemory >>= \memory' ->
            if Map.member pointer' memory'
                then setMemory (Map.update (const $ Just newValue) pointer' memory')
                else setMemory (Map.insert pointer' newValue memory')

appendToOutput :: Char -> State BrainfuckState ()
appendToOutput c = get >>= put . over output (`Text.snoc` c)

currentMemory :: State BrainfuckState (Map Int Int)
currentMemory = get >>= \state -> pure (state ^. memory)

setMemory :: Map Int Int -> State BrainfuckState ()
setMemory newMemory = get >>= put . set memory newMemory
