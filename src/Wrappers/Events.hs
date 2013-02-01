{-# OPTIONS -fno-cse #-}
{-# LANGUAGE NoImplicitPrelude
           #-}
-- | Abstract event wrapper
module Wrappers.Events ( Event (..)
                       , Button (..)
                       , GLFW.Key (..)
                       , GLFW.MouseButton (..)
                       , Size (..)
                       , ButtonState (..)
                       , initEvents
                       , popEvent
                       ) where

import Prelewd

import IO

import Text.Show
import System.IO.Unsafe

import Wrappers.OpenGL
import Wrappers.STM

import qualified Graphics.UI.GLFW as GLFW

-- | The state of an input device button
data ButtonState = Press | Release
    deriving (Show, Eq)

btnState :: Bool -> ButtonState
btnState True = Press
btnState False = Release

-- | Event data structure dictates what events we can accept
data Event = ButtonEvent Button ButtonState
           | MouseMoveEvent Position
           | ResizeEvent Size
           | RefreshEvent
           | CloseEvent
    deriving (Eq, Show)

-- | Buttons can be keys or mouse presses
data Button = KeyButton GLFW.Key
            | MouseButton GLFW.MouseButton
    deriving (Eq, Show)

events :: TQueue Event
events = unsafePerformIO newTQueueIO

{-# NOINLINE events #-}

-- | Push an event into the shared variable.
addEvent :: Event -> IO ()
addEvent = atomically . writeTQueue events

-- | Set up a queued event system
-- `GLFW.initialize` must have been called
initEvents :: IO ()
initEvents = filter id (io GLFW.initialize) >> io setCallbacks
    where
        toSize = on Size fromIntegral
        toPos = on Position fromIntegral

        setCallbacks = sequence_
            [ GLFW.setWindowCloseCallback $ True <$ (runIO $ addEvent CloseEvent)
            , GLFW.setWindowSizeCallback $ runIO . addEvent . ResizeEvent <$$> toSize
            , GLFW.setWindowRefreshCallback $ runIO (addEvent RefreshEvent)
            , GLFW.setKeyCallback $ runIO . addEvent <$$> ButtonEvent . KeyButton <%> btnState
            , GLFW.setMouseButtonCallback $ runIO . addEvent <$$> ButtonEvent . MouseButton <%> btnState
            , GLFW.setMousePositionCallback $ runIO . addEvent . MouseMoveEvent <$$> toPos
            ]

popEvent :: IO (Maybe Event)
popEvent = atomically $ tryReadTQueue events
