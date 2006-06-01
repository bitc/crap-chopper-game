module GraphicsUtil
(
glVector3FromVector3,
glVertex3FromVector3
)
where

import qualified Graphics.Rendering.OpenGL as GL

import Vector3

glVector3FromVector3 :: Vector3 -> GL.Vector3 Double
glVector3FromVector3 v = GL.Vector3 (vector3X v) (vector3Y v) (vector3Z v)

glVertex3FromVector3 :: Vector3 -> GL.Vertex3 Double
glVertex3FromVector3 v = GL.Vertex3 (vector3X v) (vector3Y v) (vector3Z v)

