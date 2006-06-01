module Graphics
(
initGraphics,
screenCoordsGroundIntersect,
renderFPS,
render
)
where

import qualified Graphics.Rendering.OpenGL as GL

import GameState
import Chopper
import ChopperBullet
import GraphicsUtil
import Vector3
import Math
import TextOutput
import StupidEnemy

fieldOfView :: Double
fieldOfView = 45
windowAspectRatio :: Double
windowAspectRatio = 4 / 3
minDepth, maxDepth :: Double
(minDepth, maxDepth) = (0.1, 100)

initGraphics :: IO ()
initGraphics = do
  GL.depthFunc GL.$= Just GL.Lequal
  GL.clearColor GL.$= GL.Color4 0 0 0 0
  GL.loadIdentity
  GL.matrixMode GL.$= GL.Projection
  GL.loadIdentity
  GL.perspective fieldOfView windowAspectRatio minDepth maxDepth
  GL.matrixMode GL.$= GL.Modelview 0
  GL.cullFace GL.$= Just GL.Back

-- TODO also rotate intersection ray by cameraYaw
screenCoordsGroundIntersect :: (Double, Double) -> Vector3
screenCoordsGroundIntersect (screenx, screeny) =
  let xr = ((screenx*2) - 1) * windowAspectRatio
      yr = - (screeny*2) + 1
      ray0 = vector3 xr yr (-2.4)
      ray1 = rotateRayX ray0 (negate (cameraAngle))
      n = cameraHeight / (negate (vector3Y ray1))
      rotateRayX :: Vector3 -> Double -> Vector3
      rotateRayX r a =
        let y = vector3Y r
            z = vector3Z r
            y' = (cos a) * y - (sin a) * z
            z' = (cos a) * z + (sin a) * y in
        vector3 (vector3X r) y' z'
      rotateRayY :: Vector3 -> Double -> Vector3
      rotateRayY r a =
        let x = vector3X r
            z = vector3Z r
            x' = (cos a) * x - (sin a) * z
            z' = (cos a) * z + (sin a) * x in
        vector3 x' (vector3Y r) z' in
  (vector3 0 cameraHeight 0) ^+^ (n *^ ray1)

cameraAngle, cameraHeight :: Double
cameraAngle = fromDegrees 40
cameraHeight = 10 * (sin (pi/2 - cameraAngle))

renderFPS :: Double -> IO ()
renderFPS fps = do
  GL.depthFunc GL.$= Nothing
  GL.matrixMode GL.$= GL.Projection
  GL.preservingMatrix $ do
    GL.loadIdentity
    GL.ortho (-1) 1 (-0.75) 0.75 (-1) 1
    GL.matrixMode GL.$= GL.Modelview 0
    GL.preservingMatrix $ do
      GL.loadIdentity
      GL.translate (GL.Vector3 (-1 + charSpacing*charWidth) (0.75 - charHeight - charSpacing*charWidth) 0)
      GL.color (GL.Color3 (1::Double) 1 1)
      GL.renderPrimitive
          GL.Lines
          (foldl1 (>>) (map ( \ (x, y) -> GL.vertex (GL.Vertex2 x y) ) (map ( \ (x, y) -> (x*charWidth, y*charHeight) ) (glyphFromString charSpacing (show $ floor fps)))))
    GL.matrixMode GL.$= GL.Projection
  GL.matrixMode GL.$= GL.Modelview 0
  GL.depthFunc GL.$= Just GL.Lequal
  where charWidth = 0.02
        charHeight = 0.04
        charSpacing = 0.3

render :: GameState -> IO ()
render gs = do
  -- Clear OpenGL Frame Buffer
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]

  -- Reset OpenGL Modelview Matrix
  GL.loadIdentity

  -- Transform Camera
  GL.rotate (toDegrees cameraAngle) (GL.Vector3 (1::Double) 0 0)
  GL.rotate (vector3X (Chopper.getPos (getChopper gs))) (GL.Vector3 0 1 0)
  GL.translate (GL.Vector3 (0::Double) (-cameraHeight) 0)


  -- Render Ground
  let scroll = getScroll gs

  GL.preservingMatrix
      (GL.translate (GL.Vector3 (0::Double) 0 (scroll - (fromIntegral ((floor scroll)::Int)))) >>
       renderGround)

  -- Render Chopper
  GL.preservingMatrix $ renderChopper (getChopper gs)

  -- Render Chopper Bullets
  mapM_ renderChopperBullet (getChopperBullets gs)

  -- Render Stupid Enemies
  mapM_ (GL.preservingMatrix . renderStupidEnemy) (getStupidEnemies gs)

  -- Render shit text
  {-
  GL.translate (glVector3FromVector3 (vector3 (-0.15 * (fromIntegral (length "BASE ARE BELONG TO US"))) 5 (-5)))
  GL.color (GL.Color3 (0::Double) 1 0)
  GL.renderPrimitive
    GL.Lines
    (foldl1 (>>) (map ( \ (x, y, z) -> GL.vertex (GL.Vertex3 x y z) ) (map ( \ (x, y) -> (x*0.25, y*0.5, 0) ) (glyphFromString 0.2 "BASE ARE BELONG TO US"))))
  -}


renderGround :: IO ()
renderGround = do
  GL.color (GL.Color3 (0.3::Double) 0.3 0.3)
  GL.renderPrimitive
      GL.Lines
      ((mapM_ ( \x -> GL.vertex (GL.Vertex3 (-15::Double) 0 x) >>
                      GL.vertex (GL.Vertex3 (15::Double) 0 x) )
              (map ( \x -> x * 1 ) [(-40)..40]))
      >>
      (mapM_ ( \x -> GL.vertex (GL.Vertex3 x 0 (-40::Double)) >>
                     GL.vertex (GL.Vertex3 x 0 (40::Double)) )
             (map ( \x -> x * 1 ) [(-15)..15])))

  return ()

renderChopper :: Chopper -> IO ()
renderChopper chopper = do
  GL.color (GL.Color3 (0::Double) 1 0)
  GL.renderPrimitive
      GL.Lines
      (GL.vertex (GL.Vertex3 (vector3X (Chopper.getPos (chopper)))
                             (vector3Y (Chopper.getPos (chopper)))
                             (vector3Z (Chopper.getPos (chopper)))) >>
       GL.vertex (GL.Vertex3 (vector3X (Chopper.getPos (chopper)))
                             0
                             (vector3Z (Chopper.getPos (chopper)))))

  GL.preservingMatrix $ do
    GL.color (GL.Color3 (1::Double) 0.5 0)
    GL.translate (glVector3FromVector3 (getTarget chopper))
    GL.renderPrimitive
        GL.LineStrip
        (foldl1 (>>) (map ( \ (x, y, z) -> GL.vertex (GL.Vertex3 (x::Double) y z) )
                          (let (r, n) = (0.6, 12) in
                           map (\i -> ((cos (2*pi/n*i))*r, (0),
                                       (sin (2*pi/n*i))*r)) [0..n])))

  GL.translate (glVector3FromVector3 (Chopper.getPos chopper))
  GL.rotate (getRoll chopper) (GL.Vector3 0 0 1)

  GL.color (GL.Color3 (1::Double) 0 0)
  GL.renderPrimitive
      GL.Lines
      (foldl1 (>>) (map ( \ (x, y, z) -> GL.vertex (GL.Vertex3 x y z) ) lineList))

  GL.preservingMatrix $ do
    GL.translate (GL.Vector3 0 (cabin + 0.1) 0)
    GL.rotate (getPropellerSpin chopper) (GL.Vector3 0 1 0)
    GL.renderPrimitive
        GL.Lines
        (foldl1 (>>) (map ( \ (x, y, z) -> GL.vertex (GL.Vertex3 x y z) ) propellerLineList))

  GL.preservingMatrix $ do
    GL.translate (GL.Vector3 0.05 (-cabin) (cabin+tailLength))
    GL.rotate (getPropellerSpin chopper) (GL.Vector3 1 0 0)
    GL.renderPrimitive
        GL.Lines
        (foldl1 (>>) (map ( \ (x, y, z) -> GL.vertex (GL.Vertex3 x y z) ) miniPropellerLineList))


  where lineList :: [(Double, Double, Double)]
        lineList = [
          (-cabin, -cabin, -cabin), ( cabin, -cabin, -cabin),
          (-cabin, -cabin, -cabin), (-cabin,  cabin, -cabin),
          (-cabin, -cabin, -cabin), (-cabin, -cabin,  cabin),
          ( cabin,  cabin,  cabin), (-cabin,  cabin,  cabin),
          ( cabin,  cabin,  cabin), ( cabin, -cabin,  cabin),
          ( cabin,  cabin,  cabin), ( cabin,  cabin, -cabin),
          ( cabin,  cabin, -cabin), (-cabin,  cabin, -cabin),
          ( cabin,  cabin, -cabin), ( cabin, -cabin, -cabin),
          ( cabin, -cabin,  cabin), (-cabin, -cabin,  cabin),
          ( cabin, -cabin,  cabin), ( cabin, -cabin, -cabin),
          (-cabin,  cabin,  cabin), (-cabin, -cabin,  cabin),
          (-cabin,  cabin,  cabin), (-cabin,  cabin, -cabin),
          (-tailWidth, -cabin, cabin), (0, -cabin, cabin+tailLength),
          ( tailWidth, -cabin, cabin), (0, -cabin, cabin+tailLength),
          (0, 0, cabin), (0, -cabin, cabin+tailLength),
          (-tailWidth, -cabin, cabin), (0, 0, cabin),
          ( tailWidth, -cabin, cabin), (0, 0, cabin)
          ]
        cabin = 0.3
        tailLength = 1.2
        tailWidth = 0.2

        propellerLineList :: [(Double, Double, Double)]
        propellerLineList = [(-1, 0, 0), (1, 0, 0), (0, 0, -1), (0, 0, 1)]

        miniPropellerLineList :: [(Double, Double, Double)]
        miniPropellerLineList = [(0, -0.3, 0), (0, 0.3, 0), (0, 0, -0.3), (0, 0, 0.3)]

renderChopperBullet :: ChopperBullet -> IO ()
renderChopperBullet cb =
  let end1, end2 :: Vector3
      end1 = ChopperBullet.getPos cb
      end2 = (ChopperBullet.getPos cb) ^-^ (bulletLength *^ (getDir cb))
  in do
  GL.color $ GL.Color3 (1::Double) 1 0
  GL.renderPrimitive
    GL.Lines
    (GL.vertex (glVertex3FromVector3 end1) >>
     GL.vertex (glVertex3FromVector3 end2))
  where bulletLength = 1

renderStupidEnemy :: StupidEnemy -> IO ()
renderStupidEnemy se = do
  GL.translate (glVector3FromVector3 (StupidEnemy.getPos se))
  GL.color (GL.Color3 (0::Double) 1 1)
  GL.renderPrimitive
      GL.Lines
      (foldl1 (>>) (map ( \ (x, y, z) -> GL.vertex (GL.Vertex3 x y z) ) lineList))
  where lineList :: [(Double, Double, Double)]
        lineList = [
          (-side, -side, -side), ( side, -side, -side),
          (-side, -side, -side), (-side,  side, -side),
          (-side, -side, -side), (-side, -side,  side),
          ( side,  side,  side), (-side,  side,  side),
          ( side,  side,  side), ( side, -side,  side),
          ( side,  side,  side), ( side,  side, -side),
          ( side,  side, -side), (-side,  side, -side),
          ( side,  side, -side), ( side, -side, -side),
          ( side, -side,  side), (-side, -side,  side),
          ( side, -side,  side), ( side, -side, -side),
          (-side,  side,  side), (-side, -side,  side),
          (-side,  side,  side), (-side,  side, -side)]
        side = 0.4

