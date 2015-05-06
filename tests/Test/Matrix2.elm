module Test.Matrix2
 ( suite_matrix2
 ) where


import Matrix2 exposing (..)
import Check exposing (..)
import Check.Investigator exposing (..)


matrix2 : Investigator Matrix2
matrix2 =
  tuple
    ( tuple (float, float)
    , tuple (float, float)
    )


suite_matrix2 =
  suite "Matrix2 Suite"
    [ suite_arithmetic
    , suite_laws
    ]


suite_arithmetic =
  suite "Arithmetic claims"
    [ claim_multiply_identity_noop
    ]

suite_laws =
  suite "Matrix2 Laws"
    [ claim_transpose_twice_original
    , claim_inverse_twice_original
    ]


claim_multiply_identity_noop =
  claim
    "Multiplying by the identity matrix yields the original matrix"
  `that`
    (mul eye)
  `is`
    identity
  `for`
    matrix2

claim_transpose_twice_original =
  claim
    "Transposing a matrix twice yields the original matrix"
  `that`
    (transpose >> transpose)
  `is`
    identity
  `for`
    matrix2


claim_inverse_twice_original =
  claim
    "Inversing a matrix twice yields the original matrix"
  `true`
    (\m -> if isInvertible m then True else inverse (inverse m) `approx` m)
  `for`
    matrix2
