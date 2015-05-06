module Test.Vector2
  ( suite_vector2
  ) where


import Vector2 exposing (..)
import Check exposing (..)
import Check.Investigator exposing (..)

vector2 : Investigator Vector2
vector2 = tuple (float, float)

----------------
-- Main Suite --
----------------

suite_vector2 =
  suite "Vector2 Claims"
    [ suite_arithmetic
    , suite_laws
    ]


-----------------
-- Mini Suites --
-----------------

suite_arithmetic =
  suite "Arithmetic Claims"
    [ claim_addition_commutative
    , claim_addition_associative
    , claim_addition_zero_noop
    , claim_addition_subtraction_inverse
    , claim_scale_by_zero_yields_zero
    , claim_adding_negation_zero
    ]


suite_laws =
  suite "Vector Laws"
    [ claim_vector_length_nonnegative
    , claim_length_squared_dot_product_equivalence
    , claim_length_normalized_vector_one
    , claim_length_direction_one
    ]

---------------------
-- Arithmetic Claims --
---------------------


claim_addition_commutative =
  claim
    "Addition is commutative"
  `that`
    (\(p,q) -> q `add` p)
  `is`
    (\(p,q) -> p `add` q)
  `for`
    tuple (vector2, vector2)


claim_addition_zero_noop =
  claim
    "Addition by zero is a noop"
  `that`
    (\p -> p `add` zero)
  `is`
    identity
  `for`
    vector2



claim_addition_associative =
  claim
    "Addition is associative"
  `true`
    (\(p, q, r) ->
      (p `add` (q `add` r)) `approx`
      ((p `add` q) `add` r))
  `for`
    tuple3 (vector2, vector2, vector2)




claim_addition_subtraction_inverse =
  claim
    "Addition and subtraction are inverse operations"
  `true`
    (\(p, q) -> p `add` q `sub` q `approx` p)
  `for`
    tuple (vector2, vector2)

claim_scale_by_zero_yields_zero =
  claim
    "Scaling by zero yields the zero vector"
  `that`
    scale 0
  `is`
    always zero
  `for`
    vector2


claim_adding_negation_zero =
  claim
    "Adding a vector to its negation yields the zero vector"
  `that`
    (\p -> p `add` neg p)
  `is`
    always zero
  `for`
    vector2

----------
-- LAWS --
----------

claim_vector_length_nonnegative =
  claim
    "Vector length is strictly nonnegative"
  `true`
    (\p -> length p >= 0)
  `for`
    vector2


claim_length_squared_dot_product_equivalence =
  claim
    "The length squared of a vector is equal to the vector dotted with itself"
  `that`
    (\p -> lengthSquared p)
  `is`
    (\p -> p `dot` p)
  `for`
    vector2



claim_length_normalized_vector_one =
  claim
    "The length of a normalized vector is always one"
  `true`
    (\p ->
      if p == zero
      then
        length (normalize p) == 0
      else
        abs (length (normalize p) - 1) < epsilon
    )
  `for`
    vector2



claim_length_direction_one =
  claim
    "The length of the direction vector computed between two vectors is always one"
  `true`
    (\(p, q) ->
        let lenDir = length (direction p q)
        in
          if p == zero && q == zero
          then
            lenDir == 0
          else
            abs (lenDir - 1) < epsilon
    )
  `for`
    tuple (vector2, vector2)
