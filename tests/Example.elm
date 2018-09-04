module Example exposing (suite)

import Expect exposing (Expectation, FloatingPointTolerance(..))
import Fuzz exposing (Fuzzer, int, list, string)
import Render exposing (Tuple)
import Test exposing (..)


floatEquals =
    Expect.within (Absolute 0.000000001)


suite : Test
suite =
    describe "A Tuple"
        [ test "with w=1.0 is a point" <|
            \_ ->
                let
                    tuple =
                        Tuple 4.3 -4.2 3.1 1.0
                in
                Expect.all
                    [ .x >> floatEquals 4.3
                    , .y >> floatEquals -4.2
                    , .z >> floatEquals 3.1
                    , .w >> floatEquals 1.0
                    , Render.isPoint >> Expect.true "is point"
                    , Render.isVector >> Expect.false "is not vector"
                    ]
                    tuple
        , test "with w=0 is a vector" <|
            \_ ->
                let
                    tuple =
                        Tuple 4.3 -4.2 3.1 0.0
                in
                Expect.all
                    [ .x >> floatEquals 4.3
                    , .y >> floatEquals -4.2
                    , .z >> floatEquals 3.1
                    , .w >> floatEquals 0.0
                    , Render.isPoint >> Expect.false "is not point"
                    , Render.isVector >> Expect.true "is vector"
                    ]
                    tuple
        ]
