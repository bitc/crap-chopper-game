module Window
(
Window,
createWindow,
windowCoordToScreenCoord
)
where

import qualified Graphics.UI.SDL as SDL

type Window = (Int, Int)

createWindow :: Int -> Int -> Bool -> IO Window
createWindow width height fullscreen = do
  --SDL.glSetAttribute SDL.glRedSize 5
  --SDL.glSetAttribute SDL.glGreenSize 5
  --SDL.glSetAttribute SDL.glBlueSize 5
  SDL.glSetAttribute SDL.glDepthSize 16
  SDL.glSetAttribute SDL.glDoubleBuffer 1

  SDL.setVideoMode width height 0 (SDL.OpenGL : (if fullscreen
                                                then [SDL.Fullscreen]
                                                else []))

  SDL.setCaption "x" ""

  SDL.showCursor False

  return (width, height)

windowCoordToScreenCoord :: Window -> (Int, Int) -> (Double, Double)
windowCoordToScreenCoord (windowWidth, windowHeight) (x, y) =
  (min ((fromIntegral x)*mouseSpeedMultiplier / fromIntegral windowWidth) 1.0,
   min ((fromIntegral y)*mouseSpeedMultiplier / fromIntegral windowHeight) 1.0)
  where mouseSpeedMultiplier = 1

