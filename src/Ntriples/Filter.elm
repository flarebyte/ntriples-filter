module Ntriples.Filter exposing (..)

{-| Convenience functions for filtering list of ntriples

# Basics
@docs  Triple, FieldId, FieldComparator, FilterExpr, filterTriples

-}
import List exposing (..)
import Maybe
import String

-- TYPES

{-| A rdf triple -}
type alias Triple = { subject : String, predicate : String, object: String }

{-| an id of the field -}
type FieldId = Subject | Predicate | Obj

{-| a boolean comparator for a string -}
type FieldComparator = Ignore
  | IsEmpty
  | Equals String
  | StartsWith String
  | Contains String

{-| a filter expression -}
type FilterExpr
    = Boolean Bool
    | Not FilterExpr
    | And FilterExpr FilterExpr
    | Or FilterExpr FilterExpr
    | Field FieldComparator FieldId

{-| get the string of a triple field -}
stringOfField: FieldId -> Triple -> String
stringOfField fieldId triple =
  case fieldId of
    Subject -> triple.subject
    Predicate -> triple.predicate
    Obj -> triple.object

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
    Contains ref ->
        String.contains ref value

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
    Field comp fieldId ->
      fieldCompare comp (stringOfField fieldId triple)

{-| filter a list of triples -}
filterTriples: FilterExpr -> List Triple -> List Triple
filterTriples tripleFilter list =
  List.filter (tripleCompare tripleFilter) list
