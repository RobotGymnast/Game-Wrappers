module Wrappers.GLFW ( VideoMode (..)
                     , MonitorState (..)
                     , FocusState (..)
                     , IconifyState (..)
                     , KeyState (..)
                     , MouseButtonState (..)
                     , ModifierKeys (..)
                     , Key (..)
                     , MouseButton (..)
                     , Monitor
                     , Window
                     , closeCallback
                     , focusCallback
                     , posCallback
                     , iconifyCallback
                     , resizeCallback
                     , refreshCallback
                     , keyCallback
                     , mouseButtonCallback
                     , cursorPosCallback
                     , cursorEnterCallback
                     , videoMode
                     , videoModes
                     , monitors
                     , primaryMonitor
                     , shouldClose
                     , swapBuffers
                     , iconified
                     , time
                     , windowPos
                     , windowTitle
                     , initGLFW
                     , runGLFW
                     , terminate
                     , pollEvents
                     ) where

import Control.Applicative
import Data.Maybe
import Data.StateVar

import Graphics.UI.GLFW as GLFW

setWithoutWindow :: (Window -> Maybe (Window -> r) -> IO ()) -> Window -> SettableStateVar (Maybe r)
setWithoutWindow f w = makeSettableStateVar $ \m -> f w $ (\act _-> act) <$> m

closeCallback :: Window -> SettableStateVar (Maybe (IO ()))
closeCallback = setWithoutWindow setWindowCloseCallback

focusCallback :: Window -> SettableStateVar (Maybe (FocusState -> IO ()))
focusCallback = setWithoutWindow setWindowFocusCallback

posCallback :: Window -> SettableStateVar (Maybe (Int -> Int -> IO ()))
posCallback = setWithoutWindow setWindowPosCallback

iconifyCallback :: Window -> SettableStateVar (Maybe (IconifyState -> IO ()))
iconifyCallback = setWithoutWindow setWindowIconifyCallback

resizeCallback :: Window -> SettableStateVar (Maybe (Int -> Int -> IO ()))
resizeCallback = setWithoutWindow setWindowSizeCallback

refreshCallback :: Window -> SettableStateVar (Maybe (IO ()))
refreshCallback = setWithoutWindow setWindowRefreshCallback

keyCallback :: Window -> SettableStateVar (Maybe (Key -> Int -> KeyState -> ModifierKeys -> IO ()))
keyCallback = setWithoutWindow setKeyCallback

mouseButtonCallback :: Window -> SettableStateVar (Maybe (MouseButton -> MouseButtonState -> ModifierKeys -> IO ()))
mouseButtonCallback = setWithoutWindow setMouseButtonCallback

cursorPosCallback :: Window -> SettableStateVar (Maybe (Double -> Double -> IO ()))
cursorPosCallback = setWithoutWindow setCursorPosCallback

cursorEnterCallback :: Window -> SettableStateVar (Maybe (CursorState -> IO ()))
cursorEnterCallback = setWithoutWindow setCursorEnterCallback

videoMode :: Monitor -> GettableStateVar (Maybe VideoMode)
videoMode = makeGettableStateVar . getVideoMode

videoModes :: Monitor -> GettableStateVar (Maybe [VideoMode])
videoModes = makeGettableStateVar . getVideoModes

monitors :: GettableStateVar (Maybe [Monitor])
monitors = makeGettableStateVar getMonitors

primaryMonitor :: GettableStateVar (Maybe Monitor)
primaryMonitor = makeGettableStateVar getPrimaryMonitor

shouldClose :: Window -> StateVar Bool
shouldClose = makeStateVar <$> windowShouldClose <*> setWindowShouldClose

time :: StateVar Double
time = makeStateVar (fromMaybe 0 <$> getTime) setTime

iconified :: Window -> StateVar IconifyState
iconified w = makeStateVar (getWindowIconified w)
            $ \s -> case s of
                IconifyState'Iconified -> restoreWindow w
                IconifyState'NotIconified -> iconifyWindow w

windowPos :: Window -> StateVar (Int, Int)
windowPos w = makeStateVar (getWindowPos w) $ uncurry $ setWindowPos w

windowTitle :: Window -> SettableStateVar String
windowTitle = makeSettableStateVar . setWindowTitle

-- | Run the action within a GLFW-initialized state, and close it afterward
runGLFW :: Integral i
        => String           -- ^ Window title
        -> Maybe Monitor    -- ^ Just Monitor for fullscreen; Nothing for windowed.
        -> (i, i)           -- ^ Window position
        -> (i, i)           -- ^ Window size
        -> (Window -> IO a) -- ^ GLFW action
        -> IO (Maybe a)
runGLFW title fsmon pos dims body = do
    mw <- initGLFW title fsmon pos dims
    case mw of
      Nothing -> return Nothing
      Just w -> do
        a <- body w
        terminate
        return $ Just a

-- | Initialize GLFW. This should be run before most other GLFW commands.
initGLFW :: Integral a
         => String          -- ^ Window title
         -> Maybe Monitor   -- ^ Just Monitor for fullscreen; Nothing for windowed.
         -> (a, a)          -- ^ Window position
         -> (a, a)          -- ^ Window size
         -> IO (Maybe Window)
initGLFW title fsmon (x, y) (w, h) = do
        success <- GLFW.init
        mwnd <- createWindow (fromIntegral w) (fromIntegral h) title fsmon Nothing
        case (success, mwnd) of
          (True, Just wnd) -> do
            windowPos wnd $= (fromIntegral x, fromIntegral y)
            windowTitle wnd $= title
            return $ Just wnd
          _ -> return Nothing
