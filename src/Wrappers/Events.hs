{-# OPTIONS -fno-cse #-}
{-# LANGUAGE NoImplicitPrelude
           #-}
-- | Abstract event wrapper
module Wrappers.Events ( Event (..)
                       , ButtonState (..)
                       , GLFW.Key (..)
                       , GLFW.MouseButton (..)
                       , Size (..)
                       , initEvents
                       , popEvent
                       , events
                       ) where

import Prelewd

import Data.StateVar
import Text.Show
import System.IO
import System.IO.Unsafe

import Control.Concurrent.STM

import Wrappers.OpenGL (Position (..), Size (..))
import Wrappers.GLFW as GLFW

-- | Event data structure dictates what eventQ we can accept
data Event = ButtonEvent ButtonState ModifierKeys
           | MouseMoveEvent Position
           | ResizeEvent Size
           | RefreshEvent
           | CloseEvent
    deriving (Eq, Show, Ord)

-- | Buttons can be keys or mouse presses.
-- Ordering is arbitrary, but deterministic
data ButtonState = KeyButton GLFW.Key GLFW.KeyState
                 | MouseButton GLFW.MouseButton GLFW.MouseButtonState
    deriving (Eq, Ord, Show)

eventQ :: TQueue Event
eventQ = unsafePerformIO newTQueueIO

{-# NOINLINE eventQ #-}

-- | Push an event into the shared variable.
addEvent :: Event -> IO ()
addEvent = atomically . writeTQueue eventQ

-- | Set up a queued event system
-- `GLFW.init` must have been called
initEvents :: GLFW.Window -> IO ()
initEvents = setCallbacks
    where
        toSize = Size `on` fromIntegral
        toPos = Position `on` round

        setCallbacks w = sequence_
            [ GLFW.closeCallback w $= Just (addEvent CloseEvent)
            , GLFW.resizeCallback w $= Just (addEvent . ResizeEvent <$$> toSize)
            , GLFW.refreshCallback w $= Just (addEvent RefreshEvent)
            , GLFW.keyCallback w $= Just (\k _ s -> addEvent . ButtonEvent (KeyButton k s))
            , GLFW.mouseButtonCallback w $= Just (\b s -> addEvent . ButtonEvent (MouseButton b s))
            , GLFW.cursorPosCallback w $= Just (addEvent . MouseMoveEvent <$$> toPos)
            ]

popEvent :: IO (Maybe Event)
popEvent = atomically $ tryReadTQueue eventQ

events :: IO [Event]
events = justsM popEvent
  where
    justsM :: (Functor m, Monad m) => m (Maybe a) -> m [a]
    justsM m = m
           >>= maybe (return [])
                     (\a -> (a:) <$> justsM m)
