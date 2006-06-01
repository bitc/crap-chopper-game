module Game
(
step
)
where

import Control.Monad.State

import GameState
import Chopper
import StupidEnemy
import GameStep

scrollSpeed :: Double
scrollSpeed = 2

step :: ChopperControls -> Double -> GameState -> GameState
step chopperControls timeStep = execState $ do
  let step = GameStep timeStep scrollSpeed

  modify (stepTimer step)

  modify spawnStupidEnemies

  (modify . modifyScroll) ((+) (scrollSpeed * timeStep))
  oldChopperState <- gets getChopper
  let (spawnedChopperBullets, newChopperState) = stepChopper chopperControls timeStep oldChopperState
  modify (setChopper newChopperState)

  modify (addChopperBullets spawnedChopperBullets)

  modify (stepChopperBullets timeStep)

  modify (stepStupidEnemies step)

