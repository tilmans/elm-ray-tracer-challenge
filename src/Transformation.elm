module Transformation exposing (translation)

import Matrix exposing (..)


translation : Float -> Float -> Float -> Matrix
translation x y z =
    matrix
        [ [ 1, 0, 0, x ]
        , [ 0, 1, 0, y ]
        , [ 0, 0, 1, z ]
        , [ 0, 0, 0, 1 ]
        ]
