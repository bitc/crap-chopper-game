module StupidEnemy
(
StupidEnemy,
StupidEnemySpawnParameters,
initStupidEnemy,
getPos,
setPos,
modifyPos,
stepStupidEnemy
)
where

import Control.Monad.State

import Vector3
import GameStep

data StupidEnemy =
  StupidEnemy
  {
    sePos :: !Vector3
  }

spawnDepth = 30

type StupidEnemySpawnParameters = Double

initStupidEnemy :: StupidEnemySpawnParameters -> StupidEnemy
initStupidEnemy x =
  StupidEnemy
  {
    sePos = vector3 x 0 (-spawnDepth)
  }

makeModify :: (a -> b) -> (b -> a -> a) -> ((b -> b) -> a -> a)
makeModify gf sf modif state = sf ((modif . gf) state) state
getPos :: StupidEnemy -> Vector3
getPos = sePos
setPos :: Vector3 -> StupidEnemy -> StupidEnemy
setPos pos se = se { sePos = pos }
modifyPos :: (Vector3 -> Vector3) -> StupidEnemy -> StupidEnemy
modifyPos = makeModify getPos setPos

stepStupidEnemy :: GameStep -> StupidEnemy -> StupidEnemy
stepStupidEnemy (GameStep timeStep scrollSpeed) = execState $ do
  (modify . modifyPos) (^+^ (timeStep *^ (vector3 0 0 scrollSpeed)))

