module Math
(
fromDegrees,
toDegrees
)
where

fromDegrees :: Double -> Double
fromDegrees angle = angle * pi / 180

toDegrees :: Double -> Double
toDegrees angle = angle * 180 / pi

