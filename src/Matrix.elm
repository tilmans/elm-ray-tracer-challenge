module Matrix exposing (M2x2, M3x3, M4x4, m2x2, m3x3, m4x4, m4x4Equal, m4x4Multiply)

import Array exposing (Array)
import Utility exposing (..)


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


m2x2 : List (List Float) -> Maybe M2x2
m2x2 values =
    if List.length values /= 2 then
        Nothing

    else if
        (values
            |> List.filter (\a -> List.length a /= 2)
            |> List.length
        )
            > 0
    then
        Nothing

    else
        Just (createm2 values)


m3x3 : List (List Float) -> Maybe M3x3
m3x3 values =
    if List.length values /= 3 then
        Nothing

    else if
        (values
            |> List.filter (\a -> List.length a /= 3)
            |> List.length
        )
            > 0
    then
        Nothing

    else
        Just (createm3 values)


m4x4 : List (List Float) -> Maybe M4x4
m4x4 values =
    if List.length values /= 4 then
        Nothing

    else if
        (values
            |> List.filter (\a -> List.length a /= 4)
            |> List.length
        )
            > 0
    then
        Nothing

    else
        Just (createm4 values)


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
