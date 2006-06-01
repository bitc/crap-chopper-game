module Main
(
main
)
where

import Data.Maybe (catMaybes, isNothing)
import Control.Exception
import Control.Monad (when)
import Data.Word

import qualified Graphics.UI.SDL as SDL

import Window
import GameState
import Chopper
import Graphics
import Game

windowWidth, windowHeight :: Int
fullscreen :: Bool

--windowWidth = 1280
--windowHeight = 1024
--fullscreen = True

--windowWidth = 640
--windowHeight = 480
--fullscreen = False

big :: Bool
big = True

(windowWidth, windowHeight, fullscreen) = if big then (1280, 1024, True) else (640, 480, False)

main :: IO ()
main = do
  SDL.init [SDL.InitVideo, SDL.InitTimer]
  Control.Exception.finally
      (do window <- createWindow windowWidth windowHeight fullscreen
          initGraphics
          goLoop window)
      SDL.quit

goLoop :: Window -> IO ()
goLoop window = initDeltaTimeData >>= loop window initGameState initProcessInputData

loop :: Window -> GameState -> ProcessInputData -> DeltaTimeData -> IO ()
loop window gameState processInputData deltaTimeData = do
  input <- processInput window processInputData
  case input of
    Just (processInputData', chopperInput) -> do
      (deltaTimeData', timeStep) <- getDeltaTime deltaTimeData
      let gameState' = step chopperInput timeStep gameState
      when (timeStep > 0) $ do
        render gameState'
        renderFPS (recip timeStep)
        SDL.glSwapBuffers
      loop window gameState' processInputData' deltaTimeData'
    Nothing -> return ()

type DeltaTimeData = Word32

initDeltaTimeData :: IO DeltaTimeData
initDeltaTimeData = SDL.getTicks

getDeltaTime :: DeltaTimeData -> IO (DeltaTimeData, Double)
getDeltaTime oldTicks = do
  currentTicks <- SDL.getTicks
  let delta = fromIntegral (currentTicks - oldTicks) / 1000
  return (currentTicks, delta)

type ProcessInputData = (Bool, Bool, Bool, Bool, Bool, Bool, Int, Int, Bool)

initProcessInputData :: ProcessInputData
initProcessInputData = (False, False, False, False, False, False, 0, 0, False)

processInput :: Window -> ProcessInputData -> IO (Maybe (ProcessInputData, ChopperControls))
processInput window processInputData = do
  let eventProcessor :: SDL.Event -> (Maybe (ProcessInputData -> ProcessInputData))
      eventProcessor event = case event of
        SDL.MouseMotion x y -> Just (mouseMotion x y)
        SDL.MouseButtonDown x y SDL.ButtonLeft -> Just (mouseButton True x y)
        SDL.MouseButtonUp x y SDL.ButtonLeft -> Just (mouseButton False x y)
        SDL.KeyDown (SDL.Keysym SDL.SDLK_q _ _) -> Nothing
        SDL.KeyDown (SDL.Keysym SDL.SDLK_ESCAPE _ _) -> Nothing
        SDL.KeyDown (SDL.Keysym key _ _) -> Just (controlKey True key)
        SDL.KeyUp (SDL.Keysym key _ _) -> Just (controlKey False key)
        SDL.Quit -> Nothing
        _ -> Just id
        where
          mouseMotion x y (l, r, f, b, u, d, _, _, t) = (l, r, f, b, u, d, fromIntegral x, fromIntegral y, t)
          mouseButton p x y (l, r, f, b, u, d, _, _, _) = (l, r, f, b, u, d, fromIntegral x, fromIntegral y, p)
          controlKey p SDL.SDLK_a     (_, r, f, b, u, d, x, y, t) = (p, r, f, b, u, d, x, y, t)
          controlKey p SDL.SDLK_d     (l, _, f, b, u, d, x, y, t) = (l, p, f, b, u, d, x, y, t)
          controlKey p SDL.SDLK_w     (l, r, _, b, u, d, x, y, t) = (l, r, p, b, u, d, x, y, t)
          controlKey p SDL.SDLK_s     (l, r, f, _, u, d, x, y, t) = (l, r, f, p, u, d, x, y, t)
          controlKey p SDL.SDLK_SPACE (l, r, f, b, _, d, x, y, t) = (l, r, f, b, p, d, x, y, t)
          controlKey p SDL.SDLK_c     (l, r, f, b, u, _, x, y, t) = (l, r, f, b, u, p, x, y, t)
          controlKey _ _ x = x

  processList <- processSDLEvents eventProcessor
  let processInputData' = foldl (flip (.)) id (catMaybes processList) processInputData
  if any isNothing processList
    then return Nothing
    else return $ Just (processInputData',
                        let (l, r, f, b, u, d, x, y, t) = processInputData' in
                        ChopperControls
                        {
                          ccFlyLeft = l,
                          ccFlyRight = r,
                          ccFlyForward = f,
                          ccFlyBack = b,
                          ccFlyUp = u,
                          ccFlyDown = d,
                          ccTarget = screenCoordsGroundIntersect
                                         (windowCoordToScreenCoord window (x, y)),
                          ccTrigger = t
                         })

processSDLEvents :: (SDL.Event -> a) -> IO [a]
processSDLEvents f =
  SDL.pumpEvents >> processSDLEvents' [] >>= return . reverse where
    processSDLEvents' l = do
      event <- SDL.pollEvent
      case event of
        SDL.NoEvent -> return l
        _ -> processSDLEvents' ((f event) : l)

