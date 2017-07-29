module Ntriples.Filter exposing (Triple, TripleFilter, filterTriples)

{-| Convenience functions for filtering list of ntriples

# Basics
@docs  Triple, TripleFilter, filterTriples

-}
import List exposing (..)
import Maybe
import String

-- TYPES

{-| A rdf triple -}
type alias Triple = { subject : String, predicate : String, object: String }

{-| A rdf triple filter -}
type alias TripleFilter = { subject : Maybe String, predicate : Maybe String, object: Maybe String }

type FieldId = Subject | Predicate | Obj

type FieldComparator = Ignore
  | IsEmpty
  | Equal String
  | StartsWith String
  | Contains String

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
    Equal ref ->
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
filterTriples: TripleFilter -> List Triple -> List Triple
filterTriples tripleFilter list =
  List.filter (triplesPredicate tripleFilter) list

maybeEqual: Maybe String -> String -> Bool
maybeEqual expected actual =
  Maybe.map2 (==) expected (Just actual) |> Maybe.withDefault True

triplesPredicate: TripleFilter -> (Triple -> Bool)
triplesPredicate tripleFilter =
  \triple  -> maybeEqual tripleFilter.subject triple.subject && maybeEqual tripleFilter.predicate triple.predicate && maybeEqual tripleFilter.object triple.object
