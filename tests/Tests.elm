module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple3)
import Ntriples.Filter exposing (..)


all : Test
all =
    describe "Ntriples.Filter"
        [ describe "unique" <|
            [ test "removes duplicates" <|
                \() ->
                    Expect.equal (Ntriples.Filter.unique [ 0, 1, 1, 0, 1 ]) [ 0, 1 ]
            ]
        ]
