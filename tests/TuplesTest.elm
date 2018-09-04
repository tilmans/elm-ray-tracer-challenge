module TuplesTest exposing (suite)

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
        , test "adding two tuples" <|
            \_ ->
                let
                    a1 =
                        Tuple 3 -2 5 1

                    a2 =
                        Tuple -2 3 1 0
                in
                Tuples.add a1 a2
                    |> Tuples.equals (Tuple 1 1 6 1)
                    |> Expect.true "adds correctly"
        , test "subtracting two points" <|
            \_ ->
                let
                    p1 =
                        Tuples.point 3 2 1

                    p2 =
                        Tuples.point 5 6 7
                in
                Tuples.subtract p1 p2
                    |> Tuples.equals (Tuples.vector -2 -4 -6)
                    |> Expect.true "subtracts correctly"
        , test "subtracting a vector from a point" <|
            \_ ->
                let
                    p =
                        Tuples.point 3 2 1

                    v =
                        Tuples.vector 5 6 7
                in
                Tuples.subtract p v
                    |> Tuples.equals (Tuples.point -2 -4 -6)
                    |> Expect.true "subtracts correctly"
        , test "subtracting two vectors" <|
            \_ ->
                let
                    v1 =
                        Tuples.vector 3 2 1

                    v2 =
                        Tuples.vector 5 6 7
                in
                Tuples.subtract v1 v2
                    |> Tuples.equals (Tuples.vector -2 -4 -6)
                    |> Expect.true "subtracts correctly"
        , test "negate a tuple" <|
            \_ ->
                Tuple 1 -2 3 -4
                    |> Tuples.negate
                    |> Tuples.equals (Tuple -1 2 -3 4)
                    |> Expect.true "negates correctly"
        , test "multiply with scalar" <|
            \_ ->
                Tuple 1 -2 3 -4
                    |> Tuples.multiply 3.5
                    |> Tuples.equals (Tuple 3.5 -7 10.5 -14)
                    |> Expect.true "negates correctly"
        , test "multiply with fraction" <|
            \_ ->
                Tuple 1 -2 3 -4
                    |> Tuples.multiply 0.5
                    |> Tuples.equals (Tuple 0.5 -1 1.5 -2)
                    |> Expect.true "negates correctly"
        , test "divide with scalar" <|
            \_ ->
                Tuple 1 -2 3 -4
                    |> Tuples.divide 2
                    |> Tuples.equals (Tuple 0.5 -1 1.5 -2)
                    |> Expect.true "negates correctly"
        , test "magnitude 1 right" <|
            \_ ->
                Tuples.vector 1 0 0
                    |> Tuples.magnitude
                    |> floatEquals 1
        , test "magnitude 1 up" <|
            \_ ->
                Tuples.vector 0 1 0
                    |> Tuples.magnitude
                    |> floatEquals 1
        , test "magnitude 1 back" <|
            \_ ->
                Tuples.vector 0 0 1
                    |> Tuples.magnitude
                    |> floatEquals 1
        , test "magnitude free" <|
            \_ ->
                Tuples.vector 1 2 3
                    |> Tuples.magnitude
                    |> floatEquals (sqrt 14)
        , test "magnitude free negative" <|
            \_ ->
                Tuples.vector -1 -2 -3
                    |> Tuples.magnitude
                    |> floatEquals (sqrt 14)
        , test "normalizing 4 0 0 gives 1 0 0" <|
            \_ ->
                Tuples.vector 4 0 0
                    |> Tuples.normalize
                    |> Tuples.equals (Tuples.vector 1 0 0)
                    |> Expect.true "normalized correctly"
        , test "normalizing 1 2 3" <|
            \_ ->
                Tuples.vector 1 2 3
                    |> Tuples.normalize
                    |> Tuples.equals (Tuples.vector 0.26726 0.53452 0.80178)
                    |> Expect.true "normalized correctly"
        , test "magnitude of normal vector" <|
            \_ ->
                Tuples.vector 4 0 0
                    |> Tuples.normalize
                    |> Tuples.magnitude
                    |> floatEquals 1
        , test "dot product" <|
            \_ ->
                let
                    a =
                        Tuples.vector 1 2 3

                    b =
                        Tuples.vector 2 3 4
                in
                Tuples.dot a b
                    |> floatEquals 20
        , test "cross product" <|
            \_ ->
                let
                    v1 =
                        Tuples.vector 1 2 3

                    v2 =
                        Tuples.vector 2 3 4
                in
                Expect.all
                    [ \( a, b ) ->
                        Tuples.cross a b
                            |> Tuples.equals (Tuples.vector -1 2 -1)
                            |> Expect.true "incorrect cross"
                    , \( a, b ) ->
                        Tuples.cross b a
                            |> Tuples.equals (Tuples.vector 1 -2 1)
                            |> Expect.true "incorrect cross"
                    ]
                    ( v1, v2 )
        ]
