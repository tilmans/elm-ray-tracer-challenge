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
                    matrix
                        [ [ -3, 5 ]
                        , [ 1, -2 ]
                        ]
                of
                    M22 m ->
                        m
                            |> Expect.all
                                [ .m00 >> floatEquals -3
                                , .m01 >> floatEquals 5
                                , .m10 >> floatEquals 1
                                , .m11 >> floatEquals -2
                                ]

                    _ ->
                        fail "Can't construct matrix"
        , test "create and inspect 3x3" <|
            \_ ->
                case
                    matrix
                        [ [ -3, 5, 0 ]
                        , [ 1, -2, -7 ]
                        , [ 0, 1, 1 ]
                        ]
                of
                    M33 m3 ->
                        m3
                            |> Expect.all
                                [ .m00 >> floatEquals -3
                                , .m11 >> floatEquals -2
                                , .m22 >> floatEquals 1
                                ]

                    _ ->
                        fail "Can't construct matrix"
        , test "create and inspect 4x4" <|
            \_ ->
                case
                    matrix
                        [ [ 1, 2, 3, 4 ]
                        , [ 5.5, 6.5, 7.5, 8.5 ]
                        , [ 9, 10, 11, 12 ]
                        , [ 13.5, 14.5, 15.5, 16.5 ]
                        ]
                of
                    M44 m4 ->
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

                    _ ->
                        Expect.true "Can't construct matrix" False
        , test "equal matrices" <|
            \_ ->
                Expect.true "Equality check is wrong" <|
                    equal
                        (matrix
                            [ [ 1, 2, 3, 4 ]
                            , [ 2, 3, 4, 5 ]
                            , [ 3, 4, 5, 6 ]
                            , [ 4, 5, 6, 7 ]
                            ]
                        )
                        (matrix
                            [ [ 1, 2, 3, 4 ]
                            , [ 2, 3, 4, 5 ]
                            , [ 3, 4, 5, 6 ]
                            , [ 4, 5, 6, 7 ]
                            ]
                        )
        , test "multiply 4x" <|
            \_ ->
                multiply
                    (matrix
                        [ [ 1, 2, 3, 4 ]
                        , [ 2, 3, 4, 5 ]
                        , [ 3, 4, 5, 6 ]
                        , [ 4, 5, 6, 7 ]
                        ]
                    )
                    (matrix
                        [ [ 0, 1, 2, 4 ]
                        , [ 1, 2, 4, 8 ]
                        , [ 2, 4, 8, 16 ]
                        , [ 4, 8, 16, 32 ]
                        ]
                    )
                    |> equal
                        (matrix
                            [ [ 24, 49, 98, 196 ]
                            , [ 31, 64, 128, 256 ]
                            , [ 38, 79, 158, 316 ]
                            , [ 45, 94, 188, 376 ]
                            ]
                        )
                    |> Expect.true "Should match"
        , test "Multiply identity" <|
            \_ ->
                let
                    a =
                        matrix
                            [ [ 0, 1, 2, 4 ]
                            , [ 1, 2, 4, 8 ]
                            , [ 2, 4, 8, 16 ]
                            , [ 4, 8, 16, 32 ]
                            ]
                in
                multiply a identity4x4
                    |> equal a
                    |> Expect.true "Should match"

        -- TODO Page 76 - multiply identity with tuple (4)
        , test "transpose matrix" <|
            \_ ->
                transpose
                    (matrix
                        [ [ 0, 9, 3, 0 ]
                        , [ 9, 8, 0, 8 ]
                        , [ 1, 8, 5, 3 ]
                        , [ 0, 0, 5, 8 ]
                        ]
                    )
                    |> equal
                        (matrix
                            [ [ 0, 9, 1, 0 ]
                            , [ 9, 8, 8, 0 ]
                            , [ 3, 0, 5, 5 ]
                            , [ 0, 8, 3, 8 ]
                            ]
                        )
                    |> Expect.true "Should match"
        , test "transpose identity" <|
            \_ ->
                transpose identity4x4
                    |> equal identity4x4
                    |> Expect.true "Should match"
        , test "calculate det of 2x2" <|
            \_ ->
                matrix
                    [ [ 1, 5 ]
                    , [ -3, 2 ]
                    ]
                    |> determinant
                    |> Expect.equal 17
        , test "Submatrix of 3x3 is 2x2" <|
            \_ ->
                matrix
                    [ [ 1, 5, 0 ]
                    , [ -3, 2, 7 ]
                    , [ 0, 6, -3 ]
                    ]
                    |> submatrix 0 2
                    |> equal
                        (matrix
                            [ [ -3, 2 ]
                            , [ 0, 6 ]
                            ]
                        )
                    |> Expect.true "Does not match"
        , test "Submatrix of 4x4 is 3x3" <|
            \_ ->
                matrix
                    [ [ -6, 1, 1, 6 ]
                    , [ -8, 5, 8, 6 ]
                    , [ -1, 0, 8, 2 ]
                    , [ -7, 1, -1, 1 ]
                    ]
                    |> submatrix 2 1
                    |> equal
                        (matrix
                            [ [ -6, 1, 6 ]
                            , [ -8, 8, 6 ]
                            , [ -7, -1, 1 ]
                            ]
                        )
                    |> Expect.true "Does not match"
        , test "Minor of 3x3" <|
            \_ ->
                matrix
                    [ [ 3, 5, 0 ]
                    , [ 2, -1, -7 ]
                    , [ 6, -7, 5 ]
                    ]
                    |> minor 1 0
                    |> Expect.equal 25
        , test "Cofactor of 3x3" <|
            \_ ->
                matrix
                    [ [ 3, 5, 0 ]
                    , [ 2, -1, -7 ]
                    , [ 6, -1, 5 ]
                    ]
                    |> Expect.all
                        [ minor 0 0 >> Expect.equal -12
                        , cofactor 0 0 >> Expect.equal -12
                        , minor 1 0 >> Expect.equal 25
                        , cofactor 1 0 >> Expect.equal -25
                        ]
        , test "Determinant of 3x3" <|
            \_ ->
                matrix
                    [ [ 1, 2, 6 ]
                    , [ -5, 8, -4 ]
                    , [ 2, 6, 4 ]
                    ]
                    |> Expect.all
                        [ cofactor 0 0 >> Expect.equal 56
                        , cofactor 0 1 >> Expect.equal 12
                        , cofactor 0 2 >> Expect.equal -46
                        , determinant >> Expect.equal -196
                        ]
        , test "Determinant of 4x4" <|
            \_ ->
                matrix
                    [ [ -2, -8, 3, 5 ]
                    , [ -3, 1, 7, 3 ]
                    , [ 1, 2, -9, 6 ]
                    , [ -6, 7, 7, -9 ]
                    ]
                    |> Expect.all
                        [ cofactor 0 0 >> Expect.equal 690
                        , cofactor 0 1 >> Expect.equal 447
                        , cofactor 0 2 >> Expect.equal 210
                        , cofactor 0 3 >> Expect.equal 51
                        , determinant >> Expect.equal -4071
                        ]
        ]
