module GameState
(
GameState,
initGameState,
getScroll,
setScroll,
modifyScroll,
getChopper,
setChopper,
modifyChopper,
getChopperBullets,
addChopperBullets,
stepChopperBullets,
getStupidEnemies,
stepStupidEnemies,
stepTimer,
spawnStupidEnemies
)
where

import Data.Maybe (mapMaybe)

import Chopper
import ChopperBullet
import StupidEnemy
import GameStep

data GameState =
  GameState
  {
    gsTime :: !Double,
    gsScroll :: !Double,
    gsChopper :: !Chopper,
    gsChopperBullets :: ![ChopperBullet],
    gsStupidEnemies :: ![StupidEnemy],
    gsStupidEnemySpawnList :: ![(Double, StupidEnemySpawnParameters)]
  }

initGameState :: GameState
initGameState =
  GameState
  {
    gsTime = 0,
    gsScroll = 0,
    gsChopper = initChopper,
    gsChopperBullets = [],
    gsStupidEnemies = [],
    gsStupidEnemySpawnList = [(sqrt x, 10 * (sin (sqrt x))) | x <- [0..100]] ++
                             [(sqrt x, 5 * (sin (sqrt x))) | x <- [101,102..]]
  }

makeModify :: (a -> b) -> (b -> a -> a) -> ((b -> b) -> a -> a)
makeModify gf sf modif state = sf ((modif . gf) state) state

getScroll :: GameState -> Double
getScroll = gsScroll

setScroll :: Double -> GameState -> GameState
setScroll scroll gs = gs { gsScroll = scroll }

modifyScroll :: (Double -> Double) -> GameState -> GameState
modifyScroll = makeModify getScroll setScroll

getChopper :: GameState -> Chopper
getChopper = gsChopper

setChopper :: Chopper -> GameState -> GameState
setChopper c gs = gs { gsChopper = c }

modifyChopper :: (Chopper -> Chopper) -> GameState -> GameState
modifyChopper = makeModify getChopper setChopper

getChopperBullets :: GameState -> [ChopperBullet]
getChopperBullets = gsChopperBullets

addChopperBullets :: [ChopperBullet] -> GameState -> GameState
addChopperBullets bullets gs =
  let oldBullets = gsChopperBullets gs in
  gs { gsChopperBullets = oldBullets ++ bullets }

stepChopperBullets :: Double -> GameState -> GameState
stepChopperBullets timeStep gs =
  let oldBullets = gsChopperBullets gs
      newBullets = mapMaybe (stepChopperBullet timeStep) oldBullets in
  gs { gsChopperBullets = newBullets }

getStupidEnemies :: GameState -> [StupidEnemy]
getStupidEnemies = gsStupidEnemies

addStupidEnemies :: [StupidEnemy] -> GameState -> GameState
addStupidEnemies stupidEnemies gs =
  let oldStupidEnemies = gsStupidEnemies gs in
  gs { gsStupidEnemies = oldStupidEnemies ++ stupidEnemies }

stepStupidEnemies :: GameStep -> GameState -> GameState
stepStupidEnemies step gs =
  let oldSEs = gsStupidEnemies gs
      newSEs = map (stepStupidEnemy step) oldSEs in
  gs { gsStupidEnemies = newSEs }

stepTimer :: GameStep -> GameState -> GameState
stepTimer (GameStep timeStep _) gs =
  gs { gsTime = (gsTime gs) + timeStep }

spawnStupidEnemies :: GameState -> GameState
spawnStupidEnemies gs =
  let nextSpawnNode = head (gsStupidEnemySpawnList gs)
      nextSpawnTime = fst nextSpawnNode in
  if nextSpawnTime <= gsTime gs
  then let newSpawnedStupidEnemy = initStupidEnemy (snd nextSpawnNode)
       in spawnStupidEnemies (addStupidEnemies [newSpawnedStupidEnemy]
                                               (gs { gsStupidEnemySpawnList =
                                                     tail (gsStupidEnemySpawnList gs) }))
  else gs

