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
t7 = { subject = "s7", predicate = "code", object = "5" }
t8 = { subject = "s8", predicate = "code", object = "-5" }
t9 = { subject = "s9", predicate = "code", object = "7.4" }

allTriples = [t1, t2, t3, t4, t5, t5a, t5b, t6, t7, t8, t9]

custom: String -> String -> Bool
custom a b = a == b

all : Test
all =
    describe "Ntriples.Filter"
        [ describe "filter" <|
            [ test "filter by object value" <|
                \() ->
                    Expect.equal
                    (filter (WithObject (Equals "subject4")) allTriples)
                    [t4]
              , test "filter by object value and predicate" <|
                \() ->
                    Expect.equal
                    (filter (And (WithPredicate (Equals "name"))(WithObject (Equals "subject4")) ) allTriples)
                    [t4]
              , test "filter by Subject" <|
                \() ->
                    Expect.equal
                    (filter (WithSubject (Equals "s5")) allTriples)
                    [t5, t5a, t5b]
              , test "filter by Not Subject" <|
                \() ->
                    Expect.equal
                    (filter (Not( WithSubject (Equals "s5"))) allTriples)
                    [t1, t2, t3, t4, t6, t7, t8, t9]
              , test "filter by empty" <|
                \() ->
                    Expect.equal
                    (filter (WithObject (IsEmpty)) allTriples)
                    [t2]
              , test "filter by StartsWith" <|
                \() ->
                    Expect.equal
                    (filter (WithObject (StartsWith "some")) allTriples)
                    [t3]
              , test "filter by EndsWith" <|
                \() ->
                    Expect.equal
                    (filter (WithObject (EndsWith "1")) allTriples)
                    [t1]
              , test "filter by Contains" <|
                \() ->
                    Expect.equal
                    (filter (WithObject (Contains "ome")) allTriples)
                    [t3]
              , test "filter by Regx" <|
                \() ->
                    Expect.equal
                    (filter (WithObject (Regx (regex "ome" ))) allTriples)
                    [t3]
              , test "filter by IsTrue" <|
                \() ->
                    Expect.equal
                    (filter (WithObject (IsTrue)) allTriples)
                    [t6]
              , test "filter by EqualsAny" <|
                \() ->
                    Expect.equal
                    (filter (WithObject (EqualsAny ["subject1", "subject4"])) allTriples)
                    [t1, t4]
              , test "filter by GreaterThan" <|
                \() ->
                    Expect.equal
                    (filter (WithObject (GreaterThan 6.3)) allTriples)
                    [t9]
              , test "filter by GreaterThanOrEqual" <|
                \() ->
                    Expect.equal
                    (filter (WithObject (GreaterThanOrEqual 5)) allTriples)
                    [t7, t9]
              , test "filter by LessThan" <|
                \() ->
                    Expect.equal
                    (filter (WithObject (LessThan 0)) allTriples)
                    [t8]
              , test "filter by LessThanOrEqual" <|
                \() ->
                    Expect.equal
                    (filter (WithObject (LessThanOrEqual 5)) allTriples)
                    [t7, t8]
              , test "filter by Custom" <|
                \() ->
                    Expect.equal
                    (filter (WithObject (Custom custom "subject4")) allTriples)
                    [t4]
            ]
        ]
