module Vector2 where

type alias Vector2 = (Float, Float)

zero : Vector2
zero = (0, 0)

i : Vector2
i = (1, 0)

j : Vector2
j = (0, 1)


add : Vector2 -> Vector2 -> Vector2
add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

sub : Vector2 -> Vector2 -> Vector2
sub (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

neg : Vector2 -> Vector2
neg (x, y) = (-x, -y)

mul : Vector2 -> Vector2 -> Vector2
mul (x1, y1) (x2, y2) = (x1 * x2, y1 * y2)

div : Vector2 -> Vector2 -> Vector2
div (x1, y1) (x2, y2) = (x1 / x2, y1 / y2)

dot : Vector2 -> Vector2 -> Float
dot (x1, y1) (x2, y2) = x1 * x2 + y1 * y2

scale : Float -> Vector2 -> Vector2
scale f (x, y) = (x * f, y * f)

length : Vector2 -> Float
length v = sqrt (lengthSquared v)

lengthSquared : Vector2 -> Float
lengthSquared (x, y) = x * x + y * y

-- why 1E-9? Cuz that what it takes to pass the tests!
epsilon = 1E-9

approx : Vector2 -> Vector2 -> Bool
approx (x1, y1) (x2, y2) =
  abs (x2 - x1) + abs (y2 - y1) < epsilon



normalize : Vector2 -> Vector2
normalize (x, y) =
  if x == 0 && y == 0
  then
    (x, y)
  else
    scale (1 / (length (x,y))) (x, y)

direction : Vector2 -> Vector2 -> Vector2
direction p q =
  if p == q
  then
    zero
  else
    let
        diff = p `sub` q
    in
        scale (1 / (length diff)) diff
