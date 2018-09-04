module ColorTest exposing (suite)

import Color exposing (Color)
import Expect exposing (Expectation, FloatingPointTolerance(..))
import Test exposing (..)


floatEquals =
    Expect.within (Absolute 0.000000001)


suite : Test
suite =
    describe "A Color"
        [ test "color has all components" <|
            \_ ->
                Color -0.5 0.4 1.7
                    |> Expect.all
                        [ .red >> floatEquals -0.5
                        , .green >> floatEquals 0.4
                        , .blue >> floatEquals 1.7
                        ]
        , test "add colors" <|
            \_ ->
                let
                    a =
                        Color 0.9 0.6 0.75

                    b =
                        Color 0.7 0.1 0.25
                in
                Color.add a b
                    |> Color.equals (Color 1.6 0.7 1.0)
                    |> Expect.true "color add not correct"
        , test "subtract colors" <|
            \_ ->
                let
                    a =
                        Color 0.9 0.6 0.75

                    b =
                        Color 0.7 0.1 0.25
                in
                Color.subtract a b
                    |> Color.equals (Color 0.2 0.5 0.5)
                    |> Expect.true "color subtract not correct"
        , test "multiply color with scalar" <|
            \_ ->
                Color 0.2 0.3 0.4
                    |> Color.multiply 2
                    |> Color.equals (Color 0.4 0.6 0.8)
                    |> Expect.true "color multiply not correct"
        , test "multiply color with color" <|
            \_ ->
                let
                    a =
                        Color 1 0.2 0.4

                    b =
                        Color 0.9 1.0 0.1
                in
                Color.colorMultiply a b
                    |> Color.equals (Color 0.9 0.2 0.04)
                    |> Expect.true "color multiply not correct"
        ]
