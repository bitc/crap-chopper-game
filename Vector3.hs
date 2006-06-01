module Vector3
(
Vector3,
vector3,
vector3X,
vector3Y,
vector3Z,
vector3XYZ,
zeroVector3,
(^+^),
(^-^),
(*^),
normSquared,
norm,
normalize,
)
where

data Vector3 = Vector3 !Double !Double !Double deriving Eq

vector3 :: Double -> Double -> Double -> Vector3
vector3 x y z = Vector3 x y z

vector3X, vector3Y, vector3Z :: Vector3 -> Double
vector3X (Vector3 x _ _) = x
vector3Y (Vector3 _ y _) = y
vector3Z (Vector3 _ _ z) = z

vector3XYZ :: Vector3 -> (Double, Double, Double)
vector3XYZ (Vector3 x y z) = (x, y, z)

zeroVector3 :: Vector3
zeroVector3 = Vector3 0 0 0

(^+^) :: Vector3 -> Vector3 -> Vector3
(Vector3 x1 y1 z1) ^+^ (Vector3 x2 y2 z2) = Vector3 (x1 + x2)
                                                    (y1 + y2)
                                                    (z1 + z2)

(^-^) :: Vector3 -> Vector3 -> Vector3
(Vector3 x1 y1 z1) ^-^ (Vector3 x2 y2 z2) = Vector3 (x1 - x2)
                                                    (y1 - y2)
                                                    (z1 - z2)

(*^) :: Double -> Vector3 -> Vector3
n *^ (Vector3 x y z) = Vector3 (n * x)
                               (n * y)
                               (n * z)

normSquared :: Vector3 -> Double
normSquared (Vector3 x y z) = x*x + y*y + z*z

norm :: Vector3 -> Double
norm (Vector3 x y z) = sqrt (x*x + y*y + z*z)

normalize :: Vector3 -> Vector3
normalize v = (recip (norm v)) *^ v

