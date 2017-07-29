module Ntriples.Filter exposing (Triple, TripleFilter, filterTriples)

{-| Convenience functions for filtering list of ntriples

# Basics
@docs  Triple, TripleFilter, filterTriples

-}
import List exposing (..)
import Maybe

-- TYPES

{-| A rdf triple -}
type alias Triple = { subject : String, predicate : String, object: String }

{-| A rdf triple filter -}
type alias TripleFilter = { subject : Maybe String, predicate : Maybe String, object: Maybe String }

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
