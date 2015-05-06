module Matrix2 where

import Vector2 exposing (Vector2)

type alias Matrix2 = (Vector2, Vector2)

-- why 1E-9? Cuz that what it takes to pass the tests!
epsilon = 1E-9

approx ((m11, m12), (m21, m22)) ((n11, n12), (n21, n22)) =
  abs (n11 - m11) + abs (n12 - m12) + abs (n21 - m21) + abs (n22 - m22) < epsilon

eye : Matrix2
eye =
  ( (1, 0)
  , (0, 1)
  )


add : Matrix2 -> Matrix2 -> Matrix2
add ((m11, m12), (m21, m22)) ((n11, n12), (n21, n22)) =
  ( (m11 + n11, m12 + n12)
  , (m21 + n21, m22 + n22)
  )


sub : Matrix2 -> Matrix2 -> Matrix2
sub ((m11, m12), (m21, m22)) ((n11, n12), (n21, n22)) =
  ( (m11 - n11, m12 - n12)
  , (m21 - n21, m22 - n22)
  )


mul : Matrix2 -> Matrix2 -> Matrix2
mul ((m11, m12), (m21, m22)) ((n11, n12), (n21, n22)) =
  ( ((m11, m12) `Vector2.dot` (n11, n21), (m11, m12) `Vector2.dot` (n12, n22))
  , ((m21, m22) `Vector2.dot` (n11, n21), (m21, m22) `Vector2.dot` (n12, n22))
  )

scale : Float -> Matrix2 -> Matrix2
scale f ((a, b), (c, d)) =
  ( (a * f, b * f)
  , (c * f, d * f)
  )

det : Matrix2 -> Float
det ((a, b), (c, d)) =
  a * d - b * c


transpose : Matrix2 -> Matrix2
transpose ((m11, m12), (m21, m22)) =
  ( (m11, m21)
  , (m12, m22)
  )


inverse : Matrix2 -> Matrix2
inverse (((a, b), (c, d)) as m) =
  scale (1 / det m)
    ( (d, -b)
    , (-c, a)
    )


isInvertible : Matrix2 -> Bool
isInvertible m =
  det m == 0
