module Canvas exposing (canvas, height, read, width, write)

import Array exposing (Array)
import Color exposing (Color)


type alias Canvas =
    { width : Int, height : Int, pixel : Array (Array Color) }


canvas width_ height_ =
    Canvas width_ height_ <|
        Array.initialize width_ <|
            \_ -> Array.initialize height_ (\_ -> Color.black)


width =
    .width


height =
    .height


write : Int -> Int -> Color -> Canvas -> Canvas
write x y color canvas_ =
    let
        pixels : Array (Array Color)
        pixels =
            Array.get x canvas_.pixel
                |> (\v ->
                        case v of
                            Nothing ->
                                canvas_.pixel

                            Just row ->
                                Array.set x (Array.set y color row) canvas_.pixel
                   )
    in
    { canvas_ | pixel = pixels }


read : Int -> Int -> Canvas -> Maybe Color
read x y canvas_ =
    Array.get x canvas_.pixel |> Maybe.andThen (Array.get y)
