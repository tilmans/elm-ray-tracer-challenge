module TestUtilities exposing (equalM)

import Expect exposing (Expectation, fail, pass)
import Matrix exposing (Matrix, equal)


equalM : Matrix -> Matrix -> Expectation
equalM a b =
    if equal a b then
        pass

    else
        fail (Debug.toString a ++ "\n\nnot equal\n\n" ++ Debug.toString b)
