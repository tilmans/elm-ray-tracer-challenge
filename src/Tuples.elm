module Tuples exposing
    ( Tuple
    , add
    , cross
    , divide
    , dot
    , equals
    , isPoint
    , isVector
    , magnitude
    , multiply
    , negate
    , normalize
    , point
    , subtract
    , vector
    )

import Utility exposing (floatEquals)


type alias Tuple =
    { x : Float
    , y : Float
    , z : Float
    , w : Float
    }


isPoint : Tuple -> Bool
isPoint v =
    v.w == 1.0


isVector : Tuple -> Bool
isVector v =
    v.w == 0.0


point : Float -> Float -> Float -> Tuple
point x y z =
    Tuple x y z 1.0


vector : Float -> Float -> Float -> Tuple
vector x y z =
    Tuple x y z 0.0


add : Tuple -> Tuple -> Tuple
add a b =
    Tuple
        (a.x + b.x)
        (a.y + b.y)
        (a.z + b.z)
        (a.w + b.w)


subtract : Tuple -> Tuple -> Tuple
subtract a b =
    Tuple
        (a.x - b.x)
        (a.y - b.y)
        (a.z - b.z)
        (a.w - b.w)


negate : Tuple -> Tuple
negate a =
    Tuple
        (a.x * -1)
        (a.y * -1)
        (a.z * -1)
        (a.w * -1)


multiply : Float -> Tuple -> Tuple
multiply f a =
    Tuple
        (a.x * f)
        (a.y * f)
        (a.z * f)
        (a.w * f)


divide : Float -> Tuple -> Tuple
divide f =
    multiply (1 / f)


magnitude : Tuple -> Float
magnitude a =
    sqrt
        ((a.x ^ 2)
            + (a.y ^ 2)
            + (a.z ^ 2)
            + (a.w ^ 2)
        )


normalize : Tuple -> Tuple
normalize a =
    Tuple
        (a.x / magnitude a)
        (a.y / magnitude a)
        (a.z / magnitude a)
        (a.w / magnitude a)


dot : Tuple -> Tuple -> Float
dot a b =
    (a.x * b.x)
        + (a.y * b.y)
        + (a.z * b.z)
        + (a.w * b.w)


cross : Tuple -> Tuple -> Tuple
cross a b =
    vector
        (a.y * b.z - a.z * b.y)
        (a.z * b.x - a.x * b.z)
        (a.x * b.y - a.y * b.x)


equals : Tuple -> Tuple -> Bool
equals a b =
    floatEquals a.x b.x
        && floatEquals a.y b.y
        && floatEquals a.z b.z
        && floatEquals a.w b.w
