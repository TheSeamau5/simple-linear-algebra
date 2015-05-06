import Test.Vector2 exposing (..)
import Test.Vector3 exposing (..)
import Test.Vector4 exposing (..)
import Test.Matrix2 exposing (..)
import Check exposing (..)
import Check.Runner.Browser exposing (..)
import Random

suite_linear_algebra =
  suite "Linear Algebra Suite"
    [ suite_vector2
    , suite_vector3
    , suite_vector4
    , suite_matrix2
    ]


main =
  display (quickCheck suite_linear_algebra)
--  displayVerbose (check suite_linear_algebra 100000 (Random.initialSeed 1))
