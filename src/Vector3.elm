module Vector3 where

type alias Vector3 = (Float, Float, Float)

zero : Vector3
zero = (0, 0, 0)

i : Vector3
i = (1, 0, 0)

j : Vector3
j = (0, 1, 0)

k : Vector3
k = (0, 0, 1)


add : Vector3 -> Vector3 -> Vector3
add (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)

sub : Vector3 -> Vector3 -> Vector3
sub (x1, y1, z1) (x2, y2, z2) = (x1 - x2, y1 - y2, z1 - z2)

neg : Vector3 -> Vector3
neg (x, y, z) = (-x, -y, -z)

mul : Vector3 -> Vector3 -> Vector3
mul (x1, y1, z1) (x2, y2, z2) = (x1 * x2, y1 * y2, z1 * z2)

div : Vector3 -> Vector3 -> Vector3
div (x1, y1, z1) (x2, y2, z2) = (x1 / x2, y1 / y2, z1 / z2)

dot : Vector3 -> Vector3 -> Float
dot (x1, y1, z1) (x2, y2, z2) = x1 * x2 + y1 * y2 + z1 * z2

cross : Vector3 -> Vector3 -> Vector3
cross (x1, y1, z1) (x2, y2, z2) =
  ( y1 * z2 - z1 * y2
  , z1 * x2 - x1 * z2
  , x1 * y2 - y1 * x2
  )

tripleProduct : Vector3 -> Vector3 -> Vector3 -> Float
tripleProduct a b c = a `dot` (b `cross` c)

scale : Float -> Vector3 -> Vector3
scale f (x, y, z) = (x * f, y * f, z * f)

length : Vector3 -> Float
length v = sqrt (lengthSquared v)

lengthSquared : Vector3 -> Float
lengthSquared (x, y, z) = x * x + y * y + z * z

-- why 1E-9? Cuz that what it takes to pass the tests!
epsilon = 1E-9

approx : Vector3 -> Vector3 -> Bool
approx (x1, y1, z1) (x2, y2, z2) =
  abs (x2 - x1) + abs (y2 - y1) + abs (z2 - z1) < epsilon



normalize : Vector3 -> Vector3
normalize (x, y, z) =
  if x == 0 && y == 0 && z == 0
  then
    (x, y, z)
  else
    scale (1 / (length (x,y,z))) (x, y, z)

direction : Vector3 -> Vector3 -> Vector3
direction p q =
  if p == q
  then
    zero
  else
    let
        diff = p `sub` q
    in
        scale (1 / (length diff)) diff
