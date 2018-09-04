module Color exposing
    ( Color
    , add
    , colorMultiply
    , equals
    , multiply
    , subtract
    )

import Utility exposing (floatEquals)


type alias Color =
    { red : Float
    , green : Float
    , blue : Float
    }


add : Color -> Color -> Color
add a b =
    Color
        (a.red + b.red)
        (a.green + b.green)
        (a.blue + b.blue)


subtract : Color -> Color -> Color
subtract a b =
    Color
        (a.red - b.red)
        (a.green - b.green)
        (a.blue - b.blue)


equals : Color -> Color -> Bool
equals a b =
    floatEquals a.red b.red
        && floatEquals a.green b.green
        && floatEquals a.blue b.blue


multiply : Float -> Color -> Color
multiply f a =
    Color
        (a.red * f)
        (a.green * f)
        (a.blue * f)


colorMultiply : Color -> Color -> Color
colorMultiply a b =
    Color
        (a.red * b.red)
        (a.green * b.green)
        (a.blue * b.blue)
