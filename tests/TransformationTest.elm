module TransformationTest exposing (suite)

import Expect exposing (Expectation, FloatingPointTolerance(..))
import Matrix exposing (..)
import Test exposing (..)
import TestUtilities exposing (..)
import Transformation exposing (..)
import Tuples


suite : Test
suite =
    describe "A Transformation"
        [ test "multiply by a transformation matrix" <|
            \_ ->
                M41 (Tuples.point -3 4 5)
                    |> multiply (translation 5 -3 2)
                    |> equalM (M41 (Tuples.point 2 1 7))
        , test "multiply by the inverse of a transformation matrix" <|
            \_ ->
                M41 (Tuples.point -3 4 5)
                    |> multiply (invert (translation 5 -3 2))
                    |> equalM (M41 (Tuples.point -8 7 3))
        , test "translation does not affect vectors" <|
            \_ ->
                M41 (Tuples.vector -3 4 5)
                    |> multiply (translation 5 -3 2)
                    |> equalM (M41 (Tuples.vector -3 4 5))
        , test "scaling on point" <|
            \_ ->
                M41 (Tuples.point -4 6 8)
                    |> multiply (scaling 2 3 4)
                    |> equalM (M41 (Tuples.point -8 18 32))
        , test "scaling on vector" <|
            \_ ->
                M41 (Tuples.vector -4 6 8)
                    |> multiply (scaling 2 3 4)
                    |> equalM (M41 (Tuples.vector -8 18 32))
        , test "scaling inverse on vector" <|
            \_ ->
                M41 (Tuples.vector -4 6 8)
                    |> multiply (invert (scaling 2 3 4))
                    |> equalM (M41 (Tuples.vector -2 2 2))
        , test "reflection is scaling by negative value" <|
            \_ ->
                M41 (Tuples.vector 2 3 4)
                    |> multiply (scaling -1 1 1)
                    |> equalM (M41 (Tuples.vector -2 3 4))
        , test "Rotate around x 45 (quarter radian)" <|
            \_ ->
                M41 (Tuples.point 0 1 0)
                    |> multiply (rotation_x 0.25)
                    |> equalM (M41 (Tuples.point 0 (sqrt 2 / 2) (sqrt 2 / 2)))
        , test "Rotate around x 90 (half radian)" <|
            \_ ->
                M41 (Tuples.point 0 1 0)
                    |> multiply (rotation_x 0.5)
                    |> equalM (M41 (Tuples.point 0 0 1))
        , test "Rotate around x 45 (quarter radian) inverse" <|
            \_ ->
                M41 (Tuples.point 0 1 0)
                    |> multiply (invert (rotation_x 0.25))
                    |> equalM (M41 (Tuples.point 0 (sqrt 2 / 2) (sqrt 2 / 2 * -1)))
        , test "Rotate around y 45 (quarter radian)" <|
            \_ ->
                M41 (Tuples.point 0 0 1)
                    |> multiply (rotation_y 0.25)
                    |> equalM (M41 (Tuples.point (sqrt 2 / 2) 0 (sqrt 2 / 2)))
        , test "Rotate around y 90 (half radian)" <|
            \_ ->
                M41 (Tuples.point 0 0 1)
                    |> multiply (rotation_y 0.5)
                    |> equalM (M41 (Tuples.point 1 0 0))
        , test "Rotate around z 45 (quarter radian)" <|
            \_ ->
                M41 (Tuples.point 0 1 0)
                    |> multiply (rotation_z 0.25)
                    |> equalM (M41 (Tuples.point (sqrt 2 / 2 * -1) (sqrt 2 / 2) 0))
        , test "Rotate around z 90 (half radian)" <|
            \_ ->
                M41 (Tuples.point 0 1 0)
                    |> multiply (rotation_z 0.5)
                    |> equalM (M41 (Tuples.point -1 0 0))
        ]
