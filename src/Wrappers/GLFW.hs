{-# LANGUAGE NoImplicitPrelude
           #-}
module Wrappers.GLFW ( DisplayOptions (..)
                     , DisplayMode (..)
                     , VideoMode (..)
                     , WindowValue (..)
                     , defaultDisplayOptions
                     , getVideoMode
                     , getVideoModes
                     , openWindow
                     , closeWindow
                     , setWindowTitle
                     , setWindowDimensions
                     , iconifyWindow
                     , restoreWindow
                     , swapBuffers
                     , setWindowBufferSwapInterval
                     , windowIsOpen
                     , windowIsActive
                     , windowIsIconified
                     , windowIsResizable
                     , windowIsHardwareAccelerated
                     , windowSupportsStereoRendering
                     , getWindowRefreshRate
                     , getWindowDimensions
                     , getWindowValue
                     , getTime
                     , setTime
                     , resetTime
                     , sleep
                     , getGlfwVersion
                     , getGlVersion
                     , initGLFW
                     , closeGLFW
                     , runGLFW
                     ) where

import Summit.Prelewd
import Summit.IO

import Graphics.UI.GLFW

-- | Run the action within a GLFW-initialized state, and close it afterward
runGLFW :: Integral a => DisplayOptions -> (a, a) -> Text -> IO b -> IO b
runGLFW opts pos title body =  initGLFW opts pos title
                            >> body
                            <* closeGLFW

-- | Initialize GLFW. This should be run before most other GLFW commands.
initGLFW :: Integral a => DisplayOptions -> (a, a) -> Text -> IO ()
initGLFW opts (x, y) title = io $ do
        True <- initialize
        True <- openWindow opts

        (setWindowPosition `on` fromIntegral) x y
        setWindowTitle title

-- | Shutdown GLFW. Most other GLFW commands should not be run after this.
closeGLFW :: IO ()
closeGLFW = io $  closeWindow
               >> terminate
