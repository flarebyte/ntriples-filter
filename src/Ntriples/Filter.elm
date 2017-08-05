module Ntriples.Filter exposing (createTriple, filter, FilterExpr(..), FieldComparator(..))

{-| This library provides an easy way of filtering a list of simplified n-triples.
More about RDF n-triples: https://en.wikipedia.org/wiki/N-Triples

# Basics
@docs  createTriple, filter

# Definition
@docs FilterExpr, FieldComparator

-}
import List
import Maybe
import String
import Regex exposing (Regex)
import Result

-- TYPES

{-| A simplified n-triple with a subject, a predicate and an object.
It is simplified because it does not discriminate between URI, blank node, language or datatype.
More about RDF n-triples: https://en.wikipedia.org/wiki/N-Triples
-}
type alias Triple = { subject : String, predicate : String, object: String }

{-| A boolean comparator for a string that can be used as part of filter query.
This comparator can be applied to any field: either the subject (WithSubject), the predicate (WithPredicate) or the object (WithObject).

## IsEmpty
Determine if the field is empty.

    WithObject (IsEmpty)

## Equals
Determine if the field is equal to the given value.

    WithSubject (Equals "http://example.org/show/211")

## StartsWith
Determine if the field starts with the given value.

    WithSubject (StartsWith "http://example.org")

## EndsWith
Determine if the field ends with the given value.

    WithSubject (EndsWith "show/211")

## Contains
Determine if the field contains the given value.

    WithSubject (Contains "show")


## Regx
Determine if the field satisfies the given regular expression.

    WithObject (Regx (regex "[Ss]eventies" ))

## IsTrue
Determine if the field is true (contains the string "true").

    WithObject (IsTrue)

## IsFalse
Determine if the field is true (contains the string "false").

    WithObject (IsFalse)

## EqualsAny
Determine if the field is equal any of the given values.

    WithObject (EqualsAny ["subject1", "subject4"])

## GreaterThan
Determine if the field is greater than the given float value.

    WithObject (GreaterThan 6.3)
    WithObject (GreaterThan -2)

## GreaterThanOrEqual
Determine if the field is greater or equal to the given float value.

    WithObject (GreaterThanOrEqual 6.3)

## LessThan
Determine if the field is less than the given float value.

    WithObject (LessThan 0)

## LessThanOrEqual
Determine if the field is less or equal to the given float value.

    WithObject (LessThanOrEqual 6.3)

## Custom
Determine if the field satisfies a custom function against the given value.

    custom: String -> String -> Bool
    custom a b = a == b
    WithObject (Custom custom "subject4")

-}
type FieldComparator = Ignore
  | IsEmpty
  | Equals String
  | StartsWith String
  | EndsWith String
  | Contains String
  | Regx Regex
  | IsTrue
  | IsFalse
  | EqualsAny (List String)
  | GreaterThan Float
  | GreaterThanOrEqual Float
  | LessThan Float
  | LessThanOrEqual Float
  | Custom (String ->String -> Bool) String


{-| A filter expression that can be used to query the list of triples.
In short, you define your expectation (criteria) for each field (WithSubject, WithPredicate, WithObject), and you can compine these criteria by using boolean operators (Or, And, Not).

## WithSubject
Specify a criteria for the subject field.

    WithSubject (Equals "http://example.org/show/211")

## WithPredicate
Specify a criteria for the predicate field.

    WithPredicate (Equals "http://www.w3.org/2000/01/rdf-schema#label")

## WithObject
Specify a criteria for the object field.

    WithObject (Contains "this term")

## Not
Inverts the effect of a filter expression and returns a list of triples that does not match it.

    -- with subject NOT equal to http://example.org/show/211
    Not(WithSubject (Equals "http://example.org/show/211"))

## And
Joins filter expressions clauses with a logical AND returns all the triples that match the conditions of both clauses.

    -- with predicate equals "name" and object "subject4"
    And (WithPredicate (Equals "name"))(WithObject (Equals "subject4"))

## Or
Joins filter expressions clauses with a logical OR returns all the triples that match the conditions of either clauses.

    -- with object equals "subject1" or "subject4"
    filter (Or (WithObject (Equals "subject1"))(WithObject (Equals "subject4")) ) allTriples

-}
type FilterExpr
    = Boolean Bool
    | Not FilterExpr
    | And FilterExpr FilterExpr
    | Or FilterExpr FilterExpr
    | WithSubject FieldComparator
    | WithPredicate FieldComparator
    | WithObject FieldComparator

{-| compare a single field and return true if must be selected -}
fieldCompare: FieldComparator -> String -> Bool
fieldCompare comparator value =
  case comparator of
    Ignore ->
      True
    IsEmpty ->
        String.isEmpty value
    Equals ref ->
        value == ref
    StartsWith ref ->
        String.startsWith ref value
    EndsWith ref ->
        String.endsWith ref value
    Contains ref ->
        String.contains ref value
    Regx regex ->
        Regex.contains regex value
    IsTrue ->
        value == "true"
    IsFalse ->
        value == "true"
    EqualsAny list ->
        List.any (\n -> n == value) list
    GreaterThan ref ->
      Result.map (\n -> n > ref) (String.toFloat value) |> Result.withDefault False
    GreaterThanOrEqual ref ->
      Result.map (\n -> n >= ref) (String.toFloat value) |> Result.withDefault False
    LessThan ref ->
      Result.map (\n -> n < ref) (String.toFloat value) |> Result.withDefault False
    LessThanOrEqual ref ->
      Result.map (\n -> n <= ref) (String.toFloat value) |> Result.withDefault False
    Custom func ref ->
        func ref value


{-| Checks if a triple satisfies a FilterExpr -}
tripleCompare: FilterExpr -> Triple -> Bool
tripleCompare expr triple =
  case expr of
    Boolean value ->
      value
    Not expr ->
        not (tripleCompare expr triple)
    And a b ->
        (tripleCompare a triple) && (tripleCompare b triple)
    Or a b  ->
        (tripleCompare a triple) || (tripleCompare b triple)
    WithSubject comp ->
      fieldCompare comp triple.subject
    WithPredicate comp ->
      fieldCompare comp triple.predicate
    WithObject comp ->
      fieldCompare comp triple.object

{-| Create a simplified n-triple with a subject, a predicate and an object.
It is simplified because it does not discriminate between URI, blank node, language or datatype.
More about rdf n-triples. https://en.wikipedia.org/wiki/N-Triples).

    createTriple "http://example.org/show/218" "http://www.w3.org/2000/01/rdf-schema#label" "That Seventies Show"

Please note that intentionally this library has a very casual approach to the specs, and any string will be accepted.
-}
createTriple: String -> String -> String -> Triple
createTriple s p o = { subject = s, predicate = p, object = o }

{-| filter a list of n-triples based on a filter expression.

    -- Select only the triples which have a given subject
    filter (WithSubject (Equals "http://example.org/show/218")) listOfTriples

    -- Select only the triples which have a label and which starts with "That"
    filter (And (WithPredicate (Equals "http://www.w3.org/2000/01/rdf-schema#label"))(WithObject (StartsWith "That")) ) listOfTriples

-}
filter: FilterExpr -> List Triple -> List Triple
filter tripleFilter list =
  List.filter (tripleCompare tripleFilter) list
