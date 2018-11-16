module Matrix exposing
    ( Matrix(..)
    , cofactor
    , determinant
    , equal
    , identity4x4
    , matrix
    , minor
    , multiply
    , submatrix
    , transpose
    )

import Array exposing (Array)
import Utility exposing (..)


type Matrix
    = M44 M4x4
    | M33 M3x3
    | M22 M2x2
    | Illegal


type alias M2x2 =
    { m00 : Float
    , m01 : Float
    , m10 : Float
    , m11 : Float
    , m20 : Float
    , m21 : Float
    , m30 : Float
    , m31 : Float
    }


type alias M3x3 =
    { m00 : Float
    , m01 : Float
    , m02 : Float
    , m10 : Float
    , m11 : Float
    , m12 : Float
    , m20 : Float
    , m21 : Float
    , m22 : Float
    , m30 : Float
    , m31 : Float
    , m32 : Float
    }


type alias M4x4 =
    { m00 : Float
    , m01 : Float
    , m02 : Float
    , m03 : Float
    , m10 : Float
    , m11 : Float
    , m12 : Float
    , m13 : Float
    , m20 : Float
    , m21 : Float
    , m22 : Float
    , m23 : Float
    , m30 : Float
    , m31 : Float
    , m32 : Float
    , m33 : Float
    }


identity4x4 =
    matrix
        [ [ 1, 0, 0, 0 ]
        , [ 0, 1, 0, 0 ]
        , [ 0, 0, 1, 0 ]
        , [ 0, 0, 0, 1 ]
        ]


matrix : List (List Float) -> Matrix
matrix values =
    case List.length values of
        2 ->
            if sublength 2 values /= 0 then
                Illegal

            else
                M22 <| createm2 values

        3 ->
            if sublength 3 values /= 0 then
                Illegal

            else
                M33 <| createm3 values

        4 ->
            if sublength 4 values /= 0 then
                Illegal

            else
                M44 <| createm4 values

        _ ->
            Illegal


sublength target values =
    values
        |> List.filter (\a -> List.length a /= target)
        |> List.length


transpose : Matrix -> Matrix
transpose ma =
    case ma of
        M44 a ->
            matrix
                [ [ a.m00, a.m10, a.m20, a.m30 ]
                , [ a.m01, a.m11, a.m21, a.m31 ]
                , [ a.m02, a.m12, a.m22, a.m32 ]
                , [ a.m03, a.m13, a.m23, a.m33 ]
                ]

        _ ->
            Illegal


equal : Matrix -> Matrix -> Bool
equal ma mb =
    case ma of
        M22 a ->
            case mb of
                M22 b ->
                    m2x2Equal a b

                _ ->
                    False

        M33 a ->
            case mb of
                M33 b ->
                    m3x3Equal a b

                _ ->
                    False

        M44 a ->
            case mb of
                M44 b ->
                    m4x4Equal a b

                _ ->
                    False

        Illegal ->
            False


minor : Int -> Int -> Matrix -> Float
minor row col m =
    submatrix row col m
        |> determinant


cofactor : Int -> Int -> Matrix -> Float
cofactor row col m =
    let
        c =
            minor row col m
    in
    if modBy 2 (row + col) == 1 then
        c * -1

    else
        c


submatrix : Int -> Int -> Matrix -> Matrix
submatrix row col m =
    matrixToList m
        |> removeAtIndex row
        |> List.map (removeAtIndex col)
        |> matrix


matrixToList : Matrix -> List (List Float)
matrixToList m =
    case m of
        M22 a ->
            [ [ a.m00, a.m01 ]
            , [ a.m10, a.m11 ]
            ]

        M33 a ->
            [ [ a.m00, a.m01, a.m02 ]
            , [ a.m10, a.m11, a.m12 ]
            , [ a.m20, a.m21, a.m22 ]
            ]

        M44 a ->
            [ [ a.m00, a.m01, a.m02, a.m03 ]
            , [ a.m10, a.m11, a.m12, a.m13 ]
            , [ a.m20, a.m21, a.m22, a.m23 ]
            , [ a.m30, a.m31, a.m32, a.m33 ]
            ]

        _ ->
            []


removeAtIndex : Int -> List a -> List a
removeAtIndex index m =
    let
        ( _, result ) =
            List.foldl
                (\item ( i, l ) ->
                    if i /= index then
                        ( i + 1, l ++ [ item ] )

                    else
                        ( i + 1, l )
                )
                ( 0, [] )
                m
    in
    result


m2x2Equal : M2x2 -> M2x2 -> Bool
m2x2Equal a b =
    floatEquals a.m00 b.m00
        && floatEquals a.m01 b.m01
        && floatEquals a.m10 b.m10
        && floatEquals a.m11 b.m11
        && floatEquals a.m20 b.m20
        && floatEquals a.m21 b.m21
        && floatEquals a.m30 b.m30
        && floatEquals a.m31 b.m31


m3x3Equal : M3x3 -> M3x3 -> Bool
m3x3Equal a b =
    floatEquals a.m00 b.m00
        && floatEquals a.m01 b.m01
        && floatEquals a.m02 b.m02
        && floatEquals a.m10 b.m10
        && floatEquals a.m11 b.m11
        && floatEquals a.m12 b.m12
        && floatEquals a.m20 b.m20
        && floatEquals a.m21 b.m21
        && floatEquals a.m22 b.m22
        && floatEquals a.m30 b.m30
        && floatEquals a.m31 b.m31
        && floatEquals a.m32 b.m32


m4x4Equal : M4x4 -> M4x4 -> Bool
m4x4Equal a b =
    floatEquals a.m00 b.m00
        && floatEquals a.m01 b.m01
        && floatEquals a.m02 b.m02
        && floatEquals a.m03 b.m03
        && floatEquals a.m10 b.m10
        && floatEquals a.m11 b.m11
        && floatEquals a.m12 b.m12
        && floatEquals a.m13 b.m13
        && floatEquals a.m20 b.m20
        && floatEquals a.m21 b.m21
        && floatEquals a.m22 b.m22
        && floatEquals a.m23 b.m23
        && floatEquals a.m30 b.m30
        && floatEquals a.m31 b.m31
        && floatEquals a.m32 b.m32
        && floatEquals a.m33 b.m33


determinant : Matrix -> Float
determinant ma =
    case ma of
        M22 a ->
            a.m00 * a.m11 - a.m01 * a.m10

        M33 a ->
            (a.m00 * cofactor 0 0 ma)
                + (a.m01 * cofactor 0 1 ma)
                + (a.m02 * cofactor 0 2 ma)

        M44 a ->
            (a.m00 * cofactor 0 0 ma)
                + (a.m01 * cofactor 0 1 ma)
                + (a.m02 * cofactor 0 2 ma)
                + (a.m03 * cofactor 0 3 ma)

        _ ->
            0


multiply : Matrix -> Matrix -> Matrix
multiply ma mb =
    case ma of
        M44 a ->
            case mb of
                M44 b ->
                    M44 <| m4x4Multiply a b

                _ ->
                    Illegal

        _ ->
            Illegal


m4x4Multiply : M4x4 -> M4x4 -> M4x4
m4x4Multiply a b =
    M4x4
        -- 0,0
        ((a.m00 * b.m00) + (a.m01 * b.m10) + (a.m02 * b.m20) + (a.m03 * b.m30))
        -- 0,1
        ((a.m00 * b.m01) + (a.m01 * b.m11) + (a.m02 * b.m21) + (a.m03 * b.m31))
        -- 0,2
        ((a.m00 * b.m02) + (a.m01 * b.m12) + (a.m02 * b.m22) + (a.m03 * b.m32))
        -- 0,3
        ((a.m00 * b.m03) + (a.m01 * b.m13) + (a.m02 * b.m23) + (a.m03 * b.m33))
        -- 1,0
        ((a.m10 * b.m00) + (a.m11 * b.m10) + (a.m12 * b.m20) + (a.m13 * b.m30))
        -- 1,1
        ((a.m10 * b.m01) + (a.m11 * b.m11) + (a.m12 * b.m21) + (a.m13 * b.m31))
        -- 1,2
        ((a.m10 * b.m02) + (a.m11 * b.m12) + (a.m12 * b.m22) + (a.m13 * b.m32))
        -- 1,3
        ((a.m10 * b.m03) + (a.m11 * b.m13) + (a.m12 * b.m23) + (a.m13 * b.m33))
        -- 2,0
        ((a.m20 * b.m00) + (a.m21 * b.m10) + (a.m22 * b.m20) + (a.m23 * b.m30))
        -- 2,1
        ((a.m20 * b.m01) + (a.m21 * b.m11) + (a.m22 * b.m21) + (a.m23 * b.m31))
        -- 2,2
        ((a.m20 * b.m02) + (a.m21 * b.m12) + (a.m22 * b.m22) + (a.m23 * b.m32))
        -- 2,3
        ((a.m20 * b.m03) + (a.m21 * b.m13) + (a.m22 * b.m23) + (a.m23 * b.m33))
        -- 3,0
        ((a.m30 * b.m00) + (a.m31 * b.m10) + (a.m32 * b.m20) + (a.m33 * b.m30))
        -- 3,1
        ((a.m30 * b.m01) + (a.m31 * b.m11) + (a.m32 * b.m21) + (a.m33 * b.m31))
        -- 3,2
        ((a.m30 * b.m02) + (a.m31 * b.m12) + (a.m32 * b.m22) + (a.m33 * b.m32))
        -- 3,3
        ((a.m30 * b.m03) + (a.m31 * b.m13) + (a.m32 * b.m23) + (a.m33 * b.m33))


createm2 : List (List Float) -> M2x2
createm2 list =
    let
        array =
            list
                |> List.map (\row -> Array.fromList row)
                |> Array.fromList
    in
    M2x2
        (g ( 0, 0 ) array)
        (g ( 0, 1 ) array)
        (g ( 1, 0 ) array)
        (g ( 1, 1 ) array)
        (g ( 2, 0 ) array)
        (g ( 2, 1 ) array)
        (g ( 3, 0 ) array)
        (g ( 3, 1 ) array)


createm3 : List (List Float) -> M3x3
createm3 list =
    let
        array =
            list
                |> List.map (\row -> Array.fromList row)
                |> Array.fromList
    in
    M3x3
        (g ( 0, 0 ) array)
        (g ( 0, 1 ) array)
        (g ( 0, 2 ) array)
        (g ( 1, 0 ) array)
        (g ( 1, 1 ) array)
        (g ( 1, 2 ) array)
        (g ( 2, 0 ) array)
        (g ( 2, 1 ) array)
        (g ( 2, 2 ) array)
        (g ( 3, 0 ) array)
        (g ( 3, 1 ) array)
        (g ( 3, 2 ) array)


createm4 : List (List Float) -> M4x4
createm4 list =
    let
        array =
            list
                |> List.map (\row -> Array.fromList row)
                |> Array.fromList
    in
    M4x4
        (g ( 0, 0 ) array)
        (g ( 0, 1 ) array)
        (g ( 0, 2 ) array)
        (g ( 0, 3 ) array)
        (g ( 1, 0 ) array)
        (g ( 1, 1 ) array)
        (g ( 1, 2 ) array)
        (g ( 1, 3 ) array)
        (g ( 2, 0 ) array)
        (g ( 2, 1 ) array)
        (g ( 2, 2 ) array)
        (g ( 2, 3 ) array)
        (g ( 3, 0 ) array)
        (g ( 3, 1 ) array)
        (g ( 3, 2 ) array)
        (g ( 3, 3 ) array)


g : ( Int, Int ) -> Array (Array Float) -> Float
g ( x, y ) array =
    Array.get x array
        |> Maybe.andThen (Array.get y)
        |> Maybe.withDefault -999.0



--m4at ( x, y ) matrix =
--    matrix
--        |> Array.get x
--        |> Maybe.andThen (Array.get y)
