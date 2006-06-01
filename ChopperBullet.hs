module ChopperBullet
(
ChopperBullet,
initChopperBullet,
getPos,
setPos,
modifyPos,
getDir,
setDir,
modifyDir,
stepChopperBullet,
)
where

import Vector3

data ChopperBullet =
  ChopperBullet
  {
    cbPos :: !Vector3,
    cbDir :: !Vector3
  }

initChopperBullet :: Vector3 -> Vector3 -> ChopperBullet
initChopperBullet pos dir =
  ChopperBullet
  {
    cbPos = pos,
    cbDir = dir
  }

makeModify :: (a -> b) -> (b -> a -> a) -> ((b -> b) -> a -> a)
makeModify gf sf modif state = sf ((modif . gf) state) state
getPos :: ChopperBullet -> Vector3
getPos = cbPos
setPos :: Vector3 -> ChopperBullet -> ChopperBullet
setPos pos cb = cb { cbPos = pos }
modifyPos :: (Vector3 -> Vector3) -> ChopperBullet -> ChopperBullet
modifyPos = makeModify getPos setPos
getDir :: ChopperBullet -> Vector3
getDir = cbDir
setDir :: Vector3 -> ChopperBullet -> ChopperBullet
setDir dir cb = cb { cbDir = dir }
modifyDir :: (Vector3 -> Vector3) -> ChopperBullet -> ChopperBullet
modifyDir = makeModify getDir setDir

stepChopperBullet :: Double -> ChopperBullet -> Maybe ChopperBullet
stepChopperBullet timeStep oldBulletState =
  let newBulletState = modifyPos (^+^ ((bulletSpeed * timeStep) *^ (getDir oldBulletState)))
                                 oldBulletState in
  if vector3Y (getPos newBulletState) <= 0
  then Nothing
  else Just newBulletState
  where bulletSpeed = 10

