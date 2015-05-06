module Vector4 where

type alias Vector4 = (Float, Float, Float, Float)

zero : Vector4
zero = (0, 0, 0, 0)

i : Vector4
i = (1, 0, 0, 0)

j : Vector4
j = (0, 1, 0, 0)

k : Vector4
k = (0, 0, 1, 0)

l : Vector4
l = (0, 0, 0, 1)




add : Vector4 -> Vector4 -> Vector4
add (x1, y1, z1, w1) (x2, y2, z2, w2) = (x1 + x2, y1 + y2, z1 + z2, w1 + w2)

sub : Vector4 -> Vector4 -> Vector4
sub (x1, y1, z1, w1) (x2, y2, z2, w2) = (x1 - x2, y1 - y2, z1 - z2, w1 - w2)

neg : Vector4 -> Vector4
neg (x, y, z, w) = (-x, -y, -z, -w)

mul : Vector4 -> Vector4 -> Vector4
mul (x1, y1, z1, w1) (x2, y2, z2, w2) = (x1 * x2, y1 * y2, z1 * z2, w1 * w2)

div : Vector4 -> Vector4 -> Vector4
div (x1, y1, z1, w1) (x2, y2, z2, w2) = (x1 / x2, y1 / y2, z1 / z2, w1 / w2)

dot : Vector4 -> Vector4 -> Float
dot (x1, y1, z1, w1) (x2, y2, z2, w2) = x1 * x2 + y1 * y2 + z1 * z2 + w1 * w2


scale : Float -> Vector4 -> Vector4
scale f (x, y, z, w) = (x * f, y * f, z * f, w * f)

length : Vector4 -> Float
length v = sqrt (lengthSquared v)

lengthSquared : Vector4 -> Float
lengthSquared (x, y, z, w) = x * x + y * y + z * z + w * w

-- why 1E-9? Cuz that what it takes to pass the tests!
epsilon = 1E-9

approx : Vector4 -> Vector4 -> Bool
approx (x1, y1, z1, w1) (x2, y2, z2, w2) =
  abs (x2 - x1) + abs (y2 - y1) + abs (z2 - z1) + abs (w2 - w1) < epsilon



normalize : Vector4 -> Vector4
normalize (x, y, z, w) =
  if x == 0 && y == 0 && z == 0 && w == 0
  then
    (x, y, z, w)
  else
    scale (1 / (length (x,y,z,w))) (x, y, z, w)

direction : Vector4 -> Vector4 -> Vector4
direction p q =
  if p == q
  then
    zero
  else
    let
        diff = p `sub` q
    in
        scale (1 / (length diff)) diff
