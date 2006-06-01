module Chopper
(
Chopper,
initChopper,
stepChopper,
ChopperControls(..),
getPos,
setPos,
modifyPos,
setVel,
getVel,
modifyVel,
setPitch,
getPitch,
modifyPitch,
setRoll,
getRoll,
modifyRoll,
setPropellerSpin,
getPropellerSpin,
modifyPropellerSpin,
getTarget
)
where

import Control.Monad.State

import Vector3
import ChopperBullet (ChopperBullet, initChopperBullet)

data Chopper =
  Chopper
  {
    cPos :: !Vector3,
    cVel :: !Vector3,
    cPitch :: !Double,
    cRoll :: !Double,
    cPropellerSpin :: !Double,
    cTarget :: !Vector3,
    cReload :: !Double
  }

data ChopperControls =
  ChopperControls
  {
    ccFlyLeft :: Bool,
    ccFlyRight :: Bool,
    ccFlyForward :: Bool,
    ccFlyBack :: Bool,
    ccFlyUp :: Bool,
    ccFlyDown :: Bool,
    ccTarget :: Vector3,
    ccTrigger :: Bool
  }

initChopper :: Chopper
initChopper =
  Chopper
  {
    cPos = vector3 0 2 (-6),
    cVel = vector3 0 0 0,
    cPitch = 0,
    cRoll = 0,
    cPropellerSpin = 0,
    cTarget = zeroVector3, -- should be updated immediately by the first application of ChopperControls
    cReload = 0
  }

makeModify :: (a -> b) -> (b -> a -> a) -> ((b -> b) -> a -> a)
makeModify gf sf modif state = sf ((modif . gf) state) state
getPos :: Chopper -> Vector3
getPos = cPos
setPos :: Vector3 -> Chopper -> Chopper
setPos pos c = c { cPos = pos }
modifyPos :: (Vector3 -> Vector3) -> Chopper -> Chopper
modifyPos = makeModify getPos setPos
getVel :: Chopper -> Vector3
getVel = cVel
setVel :: Vector3 -> Chopper -> Chopper
setVel vel c = c { cVel = vel }
modifyVel :: (Vector3 -> Vector3) -> Chopper -> Chopper
modifyVel = makeModify getVel setVel
getPropellerSpin :: Chopper -> Double
getPropellerSpin = cPropellerSpin
setPropellerSpin :: Double -> Chopper -> Chopper
setPropellerSpin propellerSpin c = c { cPropellerSpin = propellerSpin }
modifyPropellerSpin :: (Double -> Double) -> Chopper -> Chopper
modifyPropellerSpin = makeModify getPropellerSpin setPropellerSpin
getPitch :: Chopper -> Double
getPitch = cPitch
setPitch :: Double -> Chopper -> Chopper
setPitch pitch c = c { cPitch = pitch }
modifyPitch :: (Double -> Double) -> Chopper -> Chopper
modifyPitch = makeModify getPitch setPitch
getRoll :: Chopper -> Double
getRoll = cRoll
setRoll :: Double -> Chopper -> Chopper
setRoll roll c = c { cRoll = roll }
modifyRoll :: (Double -> Double) -> Chopper -> Chopper
modifyRoll = makeModify getRoll setRoll
getTarget :: Chopper -> Vector3
getTarget = cTarget


stepChopper :: ChopperControls -> Double -> Chopper -> ([ChopperBullet], Chopper)
stepChopper controls timeStep =
  let applyAcceleration :: State Chopper ()
      applyAcceleration = do
        let flyAcceleration :: Vector3
            flyAcceleration = (flip execState) zeroVector3 $ do
              when (ccFlyLeft controls) (modify (^+^ vector3 (-speed) 0 0))
              when (ccFlyRight controls) (modify (^+^ vector3 speed 0 0))

        let airResistanceAcceleration :: Vector3
            airResistanceAcceleration = zeroVector3

        let totalAcceleration = flyAcceleration ^+^ airResistanceAcceleration

        (modify . modifyVel)
            ( \ oldVel -> oldVel ^+^ (timeStep *^ totalAcceleration) )
        where speed = 2

      applyRoll :: State Chopper ()
      applyRoll = do
        oldRoll <- gets getRoll
        when (not (ccFlyLeft controls) && not (ccFlyRight controls))
             ((modify . setRoll) $ oldRoll + (negate (signum oldRoll)) * 100 * timeStep)
        when (ccFlyLeft controls)
             ((modify . setRoll) $ min (oldRoll + 360*timeStep*rollSpeed) maxRoll)
        when (ccFlyRight controls)
             ((modify . setRoll) $ max (oldRoll - 360*timeStep*rollSpeed) (-maxRoll))
        where maxRoll = 30
              rollSpeed = 0.2

  in
  runState $ do
    applyAcceleration
    applyRoll

    newVel <- gets getVel
    (modify . modifyPos)
        ( \ oldPos -> oldPos ^+^ (timeStep *^ newVel) )

    --when (ccFlyLeft controls) $ (modify . modifyPos) (^+^ (timeStep *^ (vector3 (-1) 0 0)))
    --when (ccFlyRight controls) $ (modify . modifyPos) (^+^ (timeStep *^ (vector3 1 0 0)))

    when (ccFlyForward controls) $ (modify . modifyPos) (^+^ (timeStep *^ (vector3 0 0 (-1))))
    when (ccFlyBack controls) $ (modify . modifyPos) (^+^ (timeStep *^ (vector3 0 0 1)))
    when (ccFlyUp controls) $ (modify . modifyPos) (^+^ (timeStep *^ (vector3 0 1 0)))
    when (ccFlyDown controls) $ (modify . modifyPos) (^+^ (timeStep *^ (vector3 0 (-1) 0)))

    (modify . modifyPropellerSpin) (+ (timeStep * 360 * 2))

    (modify . ( \ t c -> c { cTarget = t } )) (ccTarget controls)

    let reloadF 0 = 0
        reloadF r = max (r - timeStep) 0

    (modify . ( \ mf c -> c { cReload = mf (cReload c) } )) reloadF

    reload <- gets cReload

    fire <- case (ccTrigger controls) && (reload == 0) of
              True -> (modify . (\ r c -> c { cReload = r } )) reloadRate >> return True
              False -> return False

    if fire
     then spawnBullet >>= \b -> return (b : [])
     else return []

    where reloadRate = 0.25

spawnBullet :: (MonadState Chopper m) => m ChopperBullet
spawnBullet = do
  pos <- gets getPos
  target <- gets getTarget
  let dir = normalize (target ^-^ pos)
  return (initChopperBullet (pos ^+^ (1 *^ dir)) dir)

