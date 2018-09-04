module Render exposing (Tuple, isPoint, isVector)


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
