{-# OPTIONS -fno-cse #-}
{-# LANGUAGE NoImplicitPrelude
           #-}
-- | Abstract event wrapper
module Wrappers.Events ( Event (..)
                       , Key (..)
                       , KeyState (..)
                       , MouseButton (..)
                       , MouseButtonState (..)
                       , Size (..)
                       , ModifierKeys (..)
                       , noModifiers
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
data Event = KeyEvent Key KeyState ModifierKeys
           | MouseButtonEvent MouseButton MouseButtonState ModifierKeys
           | MouseMoveEvent Position
           | ResizeEvent Size
           | RefreshEvent
           | CloseEvent
    deriving (Eq, Show, Ord)

noModifiers :: ModifierKeys
noModifiers = ModifierKeys False False False False

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
            , GLFW.keyCallback w $= Just (\k _ s -> addEvent . KeyEvent k s)
            , GLFW.mouseButtonCallback w $= Just (\b s -> addEvent . MouseButtonEvent b s)
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
