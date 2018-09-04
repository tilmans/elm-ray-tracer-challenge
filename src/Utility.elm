module Utility exposing (floatEquals)


floatEquals : Float -> Float -> Bool
floatEquals a b =
    abs (a - b) < 0.00001
