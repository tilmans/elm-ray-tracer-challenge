module MatrixTest exposing (floatEquals, suite)

import Expect exposing (Expectation, FloatingPointTolerance(..))
import Matrix exposing (..)
import Test exposing (..)


floatEquals =
    Expect.within (Absolute 0.000000001)


fail message =
    Expect.true message False


maybeEqual expected maybe =
    case maybe of
        Nothing ->
            fail "No Value"

        Just value ->
            floatEquals expected value


suite : Test
suite =
    describe "A Matrix"
        [ test "create and inspect 2x2" <|
            \_ ->
                case
                    m2x2
                        [ [ -3, 5 ]
                        , [ 1, -2 ]
                        ]
                of
                    Nothing ->
                        Expect.true "Can't construct matrix" False

                    Just m2 ->
                        m2
                            |> Expect.all
                                [ .m00 >> floatEquals -3
                                , .m01 >> floatEquals 5
                                , .m10 >> floatEquals 1
                                , .m11 >> floatEquals -2
                                ]
        , test "create and inspect 3x3" <|
            \_ ->
                case
                    m3x3
                        [ [ -3, 5, 0 ]
                        , [ 1, -2, -7 ]
                        , [ 0, 1, 1 ]
                        ]
                of
                    Nothing ->
                        Expect.true "Can't construct matrix" False

                    Just m3 ->
                        m3
                            |> Expect.all
                                [ .m00 >> floatEquals -3
                                , .m11 >> floatEquals -2
                                , .m22 >> floatEquals 1
                                ]
        , test "create and inspect 4x4" <|
            \_ ->
                case
                    m4x4
                        [ [ 1, 2, 3, 4 ]
                        , [ 5.5, 6.5, 7.5, 8.5 ]
                        , [ 9, 10, 11, 12 ]
                        , [ 13.5, 14.5, 15.5, 16.5 ]
                        ]
                of
                    Nothing ->
                        Expect.true "Can't construct matrix" False

                    Just m4 ->
                        m4
                            |> Expect.all
                                [ .m00 >> floatEquals 1
                                , .m03 >> floatEquals 4
                                , .m10 >> floatEquals 5.5
                                , .m12 >> floatEquals 7.5
                                , .m22 >> floatEquals 11
                                , .m30 >> floatEquals 13.5
                                , .m32 >> floatEquals 15.5
                                ]
        , test "equal matrices" <|
            \_ ->
                let
                    ma =
                        m4x4 [ [ 1, 2, 3, 4 ], [ 2, 3, 4, 5 ], [ 3, 4, 5, 6 ], [ 4, 5, 6, 7 ] ]

                    mb =
                        m4x4 [ [ 1, 2, 3, 4 ], [ 2, 3, 4, 5 ], [ 3, 4, 5, 6 ], [ 4, 5, 6, 7 ] ]
                in
                case ma of
                    Nothing ->
                        fail "Unexpected issue creating matrix"

                    Just a ->
                        case mb of
                            Nothing ->
                                fail "Unexpected issue creating matrix"

                            Just b ->
                                Expect.true "Equality check is off" <|
                                    m4x4Equal a b
        , test "multiply 4x" <|
            \_ ->
                let
                    ma =
                        m4x4
                            [ [ 1, 2, 3, 4 ]
                            , [ 2, 3, 4, 5 ]
                            , [ 3, 4, 5, 6 ]
                            , [ 4, 5, 6, 7 ]
                            ]

                    mb =
                        m4x4
                            [ [ 0, 1, 2, 3 ]
                            , [ 1, 2, 4, 8 ]
                            , [ 2, 4, 8, 16 ]
                            , [ 4, 8, 16, 32 ]
                            ]

                    mr =
                        m4x4
                            [ [ 24, 49, 98, 196 ]
                            , [ 31, 64, 128, 256 ]
                            , [ 38, 79, 158, 316 ]
                            , [ 45, 94, 188, 376 ]
                            ]
                in
                case ma of
                    Nothing ->
                        fail "Unexpected error creating matrix"

                    Just a ->
                        case mb of
                            Nothing ->
                                fail "Unexpected error creating matrix"

                            Just b ->
                                case mr of
                                    Nothing ->
                                        fail "Unexpected error creating matrix"

                                    Just r ->
                                        m4x4Multiply a b
                                            |> m4x4Equal r
                                            |> Expect.true "Should match"
        ]
