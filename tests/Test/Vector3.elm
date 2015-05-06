module Test.Vector3
  ( suite_vector3
  ) where


import Vector3 exposing (..)
import Check exposing (..)
import Check.Investigator exposing (..)

vector3 : Investigator Vector3
vector3 = tuple3 (float, float, float)

----------------
-- Main Suite --
----------------

suite_vector3 =
  suite "Vector3 Claims"
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
    , claim_triple_product_symmetric
    , claim_cross_product_anticommutative
    , claim_cross_product_distributive_over_addition
    , claim_cross_product_distributive_scalar_multiplication
    , claim_cross_product_satisfies_jacobi_identity
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
    tuple (vector3, vector3)


claim_addition_zero_noop =
  claim
    "Addition by zero is a noop"
  `that`
    (\p -> p `add` zero)
  `is`
    identity
  `for`
    vector3



claim_addition_associative =
  claim
    "Addition is associative"
  `true`
    (\(p, q, r) ->
      (p `add` (q `add` r)) `approx`
      ((p `add` q) `add` r))
  `for`
    tuple3 (vector3, vector3, vector3)




claim_addition_subtraction_inverse =
  claim
    "Addition and subtraction are inverse operations"
  `true`
    (\(p, q) -> ((p `add` q) `sub` q) `approx` p)
  `for`
    tuple (vector3, vector3)

claim_scale_by_zero_yields_zero =
  claim
    "Scaling by zero yields the zero vector"
  `that`
    scale 0
  `is`
    always zero
  `for`
    vector3


claim_adding_negation_zero =
  claim
    "Adding a vector to its negation yields the zero vector"
  `that`
    (\p -> p `add` neg p)
  `is`
    always zero
  `for`
    vector3

----------
-- LAWS --
----------

claim_vector_length_nonnegative =
  claim
    "Vector length is strictly nonnegative"
  `true`
    (\p -> length p >= 0)
  `for`
    vector3


claim_length_squared_dot_product_equivalence =
  claim
    "The length squared of a vector is equal to the vector dotted with itself"
  `that`
    (\p -> lengthSquared p)
  `is`
    (\p -> p `dot` p)
  `for`
    vector3



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
    vector3



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
    tuple (vector3, vector3)



claim_triple_product_symmetric =
  claim
    "Triple product is symmetric"
  `true`
    (\(a, b, c) -> abs (tripleProduct a b c - tripleProduct b c a) < epsilon)
  `for`
    tuple3 (vector3, vector3, vector3)


claim_cross_product_anticommutative =
  claim
    "Cross product is anti-commutative"
  `that`
    (\(a, b) -> a `cross` b)
  `is`
    (\(a, b) -> neg b `cross` a)
  `for`
    tuple (vector3, vector3)


claim_cross_product_distributive_over_addition =
  claim
    "Cross product is distributive over addition"
  `true`
    (\(a, b, c) ->
        (a `cross` (b `add` c)) `approx`
        ((a `cross` b) `add` (a `cross` c)))
  `for`
    tuple3 (vector3, vector3, vector3)


claim_cross_product_distributive_scalar_multiplication =
  claim
    "Cross product is distributive over scalar multiplication"
  `true`
    (\(r, a, b) ->
          let p1 = (scale r a) `cross` b
              p2 = a `cross` (scale r b)
              p3 = scale r (a `cross` b)
          in
            (p1 `approx` p2) &&
            (p2 `approx` p3) &&
            (p3 `approx` p1)
    )
  `for`
    tuple3 (float, vector3, vector3)


claim_cross_product_satisfies_jacobi_identity =
  claim
    "Cross product satisfies the Jacobi identity"
  `true`
    (\(a, b, c) ->
        (a `cross` (b `cross` c)) `add`
        (b `cross` (c `cross` a)) `add`
        (c `cross` (a `cross` b)) `approx` zero
    )
  `for`
    tuple3 (vector3, vector3, vector3)
