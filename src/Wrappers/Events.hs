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
                       , events
                       ) where

import Summit.Control.Stream
import Summit.IO
import Summit.Prelewd
import Summit.STM

import Text.Show
import System.IO.Unsafe

import Wrappers.OpenGL

import qualified Graphics.UI.GLFW as GLFW

-- | The state of an input device button
data ButtonState = Press | Release
    deriving (Show, Eq, Ord)

btnState :: Bool -> ButtonState
btnState True = Press
btnState False = Release

-- | Event data structure dictates what eventQ we can accept
data Event = ButtonEvent Button ButtonState
           | MouseMoveEvent Position
           | ResizeEvent Size
           | RefreshEvent
           | CloseEvent
    deriving (Eq, Show, Ord)

-- | Buttons can be keys or mouse presses.
-- Ordering is arbitrary, but deterministic
data Button = KeyButton GLFW.Key
            | MouseButton GLFW.MouseButton
    deriving (Eq, Ord, Show)

eventQ :: TQueue Event
eventQ = unsafePerformIO newTQueueIO

{-# NOINLINE eventQ #-}

-- | Push an event into the shared variable.
addEvent :: Event -> IO ()
addEvent = atomically . writeTQueue eventQ

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
popEvent = atomically $ tryReadTQueue eventQ

newEvents :: IO [Event]
newEvents = popEvent >>= ((<&> (\x -> (x:) <$> newEvents)) >>> (<?> return []))

events :: Stream IO () [Event]
events = lift $ arr $ \_-> newEvents
