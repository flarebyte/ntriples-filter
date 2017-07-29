module Tests exposing (..)

import Test exposing (..)
import List exposing (..)
import Expect
import Fuzz exposing (list, int, tuple3)
import Ntriples.Filter exposing (..)
import Maybe exposing(..)

t1 = { subject = "s1", predicate = "name", object = "subject1" }
t2 = { subject = "s2", predicate = "name", object = "subject2" }
t3 = { subject = "s3", predicate = "name", object = "subject3" }
t4 = { subject = "s4", predicate = "name", object = "subject4" }
t5 = { subject = "s5", predicate = "name", object = "subject5" }

filterSubject3: FilterExpr
filterSubject3 = Field (Equals "subject3") Obj

all : Test
all =
    describe "Ntriples.Filter"
        [ describe "filterTriples" <|
            [ test "filter by predicate object " <|
                \() ->
                    Expect.equal
                    (filterTriples filterSubject3 [t1, t2, t3])
                    [t3]
            ]
        ]
