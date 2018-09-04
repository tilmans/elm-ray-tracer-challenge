module Example exposing (suite)

import Expect exposing (Expectation, FloatingPointTolerance(..))
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Tuples exposing (Tuple)


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
                    , Tuples.isPoint >> Expect.true "is point"
                    , Tuples.isVector >> Expect.false "is not vector"
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
                    , Tuples.isPoint >> Expect.false "is not point"
                    , Tuples.isVector >> Expect.true "is vector"
                    ]
                    tuple
        , test "point describes tuple with w=1" <|
            \_ ->
                Tuples.point 4 -4 3
                    |> Tuples.equals (Tuple 4 -4 3 1.0)
                    |> Expect.true "point creates a point"
        , test "vector describes tuple with w=0" <|
            \_ ->
                Tuples.vector 4 -4 3
                    |> Tuples.equals (Tuple 4 -4 3 0.0)
                    |> Expect.true "vector creates a vector"
        ]
