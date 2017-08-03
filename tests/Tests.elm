module Tests exposing (..)

import Test exposing (describe, test, Test)
import Expect
import Ntriples.Filter exposing (..)
import Regex exposing (..)

t1 = { subject = "s1", predicate = "name", object = "subject1" }
t2 = { subject = "s2", predicate = "name", object = "" }
t3 = { subject = "s3", predicate = "name", object = "some subject3" }
t4 = { subject = "s4", predicate = "name", object = "subject4" }
t5 = { subject = "s5", predicate = "name", object = "subject5" }
t5a = { subject = "s5", predicate = "desc", object = "desc5" }
t5b = { subject = "s5", predicate = "label", object = "label5" }
t6 = { subject = "s6", predicate = "flag", object = "true" }

allTriples = [t1, t2, t3, t4, t5, t5a, t5b, t6]

all : Test
all =
    describe "Ntriples.Filter"
        [ describe "filterTriples" <|
            [ test "filter by object value" <|
                \() ->
                    Expect.equal
                    (filterTriples (WithObject (Equals "subject4")) allTriples)
                    [t4]
              , test "filter by object value and predicate" <|
                \() ->
                    Expect.equal
                    (filterTriples (And (WithPredicate (Equals "name"))(WithObject (Equals "subject4")) ) allTriples)
                    [t4]
              , test "filter by Subject" <|
                \() ->
                    Expect.equal
                    (filterTriples (WithSubject (Equals "s5")) allTriples)
                    [t5, t5a, t5b]
              , test "filter by Not Subject" <|
                \() ->
                    Expect.equal
                    (filterTriples (Not( WithSubject (Equals "s5"))) allTriples)
                    [t1, t2, t3, t4, t6]
              , test "filter by empty" <|
                \() ->
                    Expect.equal
                    (filterTriples (WithObject (IsEmpty)) allTriples)
                    [t2]
              , test "filter by StartsWith" <|
                \() ->
                    Expect.equal
                    (filterTriples (WithObject (StartsWith "some")) allTriples)
                    [t3]
              , test "filter by EndsWith" <|
                \() ->
                    Expect.equal
                    (filterTriples (WithObject (EndsWith "1")) allTriples)
                    [t1]
              , test "filter by Contains" <|
                \() ->
                    Expect.equal
                    (filterTriples (WithObject (Contains "ome")) allTriples)
                    [t3]
              , test "filter by Regx" <|
                \() ->
                    Expect.equal
                    (filterTriples (WithObject (Regx (regex "ome" ))) allTriples)
                    [t3]
              , test "filter by IsTrue" <|
                \() ->
                    Expect.equal
                    (filterTriples (WithObject (IsTrue)) allTriples)
                    [t6]
              , test "filter by EqualsAny" <|
                \() ->
                    Expect.equal
                    (filterTriples (WithObject (EqualsAny ["subject1", "subject4"])) allTriples)
                    [t1, t4]
            ]
        ]
