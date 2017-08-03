module Ntriples.Filter exposing (..)

{-| Convenience functions for filtering list of ntriples

# Basics
@docs  Triple, FieldComparator, FilterExpr, filterTriples, fieldCompare, tripleCompare

-}
import List exposing (..)
import Maybe
import String
import Regex exposing(..)

-- TYPES

{-| A rdf triple -}
type alias Triple = { subject : String, predicate : String, object: String }

{-| a boolean comparator for a string -}
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

{-| a filter expression -}
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

{-| filter a list of triples -}
filterTriples: FilterExpr -> List Triple -> List Triple
filterTriples tripleFilter list =
  List.filter (tripleCompare tripleFilter) list
