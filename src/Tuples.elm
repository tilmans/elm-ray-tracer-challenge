module Tuples exposing (Tuple, equals, isPoint, isVector, point, vector)


type alias Tuple =
    { x : Float
    , y : Float
    , z : Float
    , w : Float
    }


isPoint v =
    v.w == 1.0


isVector v =
    v.w == 0.0


point x y z =
    Tuple x y z 1.0


vector x y z =
    Tuple x y z 0.0


equals a b =
    floatEquals a.x b.x
        && floatEquals a.y b.y
        && floatEquals a.z b.z
        && floatEquals a.w b.w


floatEquals a b =
    abs (a - b) < 0.00000001
