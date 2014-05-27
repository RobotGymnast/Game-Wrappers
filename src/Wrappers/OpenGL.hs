{-# LANGUAGE FlexibleInstances #-}
-- | OpenGL with some extensions for color
module Wrappers.OpenGL ( module OGL
                       , ColorDef (..)
                       , gray
                       , drawColored
                       , GLColor (..)
                       ) where

import Control.Applicative
import Data.Foldable
import Data.Tuple.Curry

import Graphics.Rendering.OpenGL as OGL

-- | Class of types which can be used to define colors
class ColorDef c where
    red, blue, lime, forest, green, orange, yellow, cyan, pink, purple, magenta, white, black, grey, transparent :: c
    green = forest
    magenta = pink

instance ColorDef (Color4 GLdouble) where
    red         = Color4 1   0   0   1
    blue        = Color4 0   0   1   1
    lime        = Color4 0   1   0   1
    forest      = Color4 0   0.5 0   1
    orange      = Color4 1   0.5 0   1
    yellow      = Color4 1   1   0   1
    cyan        = Color4 0   1   1   1
    pink        = Color4 1   0   1   1
    purple      = Color4 0.5 0   1   1
    white       = Color4 1   1   1   1
    black       = Color4 0   0   0   1
    grey        = Color4 0.5 0.5 0.5 1
    transparent = Color4 0   0   0   0

instance ColorDef (GLubyte, GLubyte, GLubyte, GLubyte) where
    red         = (255, 0  , 0  , 255)
    blue        = (0  , 0  , 255, 255)
    lime        = (0  , 255, 0  , 255)
    forest      = (0  , 127, 0  , 255)
    orange      = (255, 127, 0  , 255)
    yellow      = (255, 255, 0  , 255)
    cyan        = (0  , 255, 255, 255)
    pink        = (255, 0  , 255, 255)
    purple      = (127, 0  , 255, 255)
    white       = (255, 255, 255, 255)
    black       = (0  , 0  , 0  , 255)
    grey        = (127, 127, 127, 255)
    transparent = (0  , 0  , 0  , 0  )

gray :: ColorDef a => a
gray = grey

-- | Set the OpenGL color, then call `vertex` on each vertex
drawColored :: (Color c, Vertex v) => c -> [v] -> IO ()
drawColored c vs = color c >> traverse_ vertex vs
 
-- | Colors which can be used to color OpenGL's rendering
class GLColor c where
    -- | Convert to an OpenGL color
    toGLColor :: c -> Color4 GLclampf

instance GLColor (Color4 GLubyte) where
    toGLColor c = (/ 255) . realToFrac <$> c

instance GLColor (Color4 GLdouble) where
    toGLColor c = realToFrac <$> c

instance GLColor (GLubyte, GLubyte, GLubyte, GLubyte) where
    toGLColor = toGLColor . uncurryN Color4
