module TransformationTest exposing (suite)

import Expect exposing (Expectation, FloatingPointTolerance(..))
import Matrix exposing (..)
import Test exposing (..)
import Transformation exposing (..)
import Tuples


suite : Test
suite =
    describe "A Transformation"
        [ test "multiply by a transformation matrix" <|
            \_ ->
                M41 (Tuples.point -3 4 5)
                    |> multiply (translation 5 -3 2)
                    |> equal (M41 (Tuples.point 2 1 7))
                    |> Expect.true "Result not as expected"
        , test "multiply by the inverse of a transformation matrix" <|
            \_ ->
                M41 (Tuples.point -3 4 5)
                    |> multiply (invert (translation 5 -3 2))
                    |> equal (M41 (Tuples.point -8 7 3))
                    |> Expect.true "Result not as expected"
        , test "translation does not affect vectors" <|
            \_ ->
                M41 (Tuples.vector -3 4 5)
                    |> multiply (translation 5 -3 2)
                    |> equal (M41 (Tuples.vector -3 4 5))
                    |> Expect.true "Result not as expected"
        ]
