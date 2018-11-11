module CanvasTest exposing (suite)

import Canvas exposing (..)
import Color
import Expect exposing (Expectation, FloatingPointTolerance(..))
import Test exposing (..)


floatEquals =
    Expect.within (Absolute 0.000000001)


suite : Test
suite =
    describe "A Canvas"
        [ test "can be created" <|
            \_ ->
                canvas 10 20
                    |> Expect.all
                        [ width >> Expect.equal 10
                        , height >> Expect.equal 20
                        ]
        , test "can write pixels" <|
            \_ ->
                let
                    red =
                        Color.black

                    pixel =
                        canvas 10 20
                            |> write 2 3 red
                            |> read 2 3
                            |> Maybe.withDefault Color.black
                in
                Expect.equal red pixel

        -- NEEDED ? Draw Canvas instead of PPM
        --, test "can construct header"
        --, text "can construct pixel data"
        --, text "split long lines"
        --, text "ppm are terminated by newline"
        ]
