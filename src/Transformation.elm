module Transformation exposing
    ( rotation_x
    , rotation_y
    , rotation_z
    , scaling
    , translation
    )

import Matrix exposing (..)


translation : Float -> Float -> Float -> Matrix
translation x y z =
    matrix
        [ [ 1, 0, 0, x ]
        , [ 0, 1, 0, y ]
        , [ 0, 0, 1, z ]
        , [ 0, 0, 0, 1 ]
        ]


scaling : Float -> Float -> Float -> Matrix
scaling x y z =
    matrix
        [ [ x, 0, 0, 0 ]
        , [ 0, y, 0, 0 ]
        , [ 0, 0, z, 0 ]
        , [ 0, 0, 0, 1 ]
        ]


rotation_x : Float -> Matrix
rotation_x r =
    matrix
        [ [ 1, 0, 0, 0 ]
        , [ 0, cos (pi * r), sin (pi * r) * -1, 0 ]
        , [ 0, sin (pi * r), cos (pi * r), 0 ]
        , [ 0, 0, 0, 1 ]
        ]


rotation_y : Float -> Matrix
rotation_y r =
    matrix
        [ [ cos (pi * r), 0, sin (pi * r), 0 ]
        , [ 0, 1, 0, 0 ]
        , [ sin (pi * r) * -1, 0, cos (pi * r), 0 ]
        , [ 0, 0, 0, 1 ]
        ]


rotation_z : Float -> Matrix
rotation_z r =
    matrix
        [ [ cos (pi * r), sin (pi * r) * -1, 0, 0 ]
        , [ sin (pi * r), cos (pi * r), 0, 0 ]
        , [ 0, 0, 1, 0 ]
        , [ 0, 0, 0, 1 ]
        ]
