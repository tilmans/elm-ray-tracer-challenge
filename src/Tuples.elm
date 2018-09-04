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
    (a.x
        == b.x
    )
        && (a.y
                == b.y
           )
        && (a.z
                == b.z
           )
        && (a.w
                == b.w
           )
