module Ntriples.Filter
    exposing
        (
        Triples
        , TriplesList
        , toNodeNames
        )

{-| Convenience functions for filtering list of ntriples

# Basics
@docs  unique, Triples, TriplesList, toNodeNames

-}
import List exposing (..)
import Graph exposing (Graph, NodeId, Node, Edge, NodeContext)


-- TYPES

{-| A rdf triple -}
type alias Triples = { subject : String, predicate : String, object: String }


{-| A list of rdf triple -}
type alias TriplesList = List Triples

type alias TriplesData =
    { list : TriplesList
    }

type alias Pathway = List String


toTriplesData: TriplesList -> TriplesData
toTriplesData list =
  { list = list
  }

{-| Remove duplicate values, keeping the first instance of each element which appears more than once.

    unique [0,1,1,0,1] == [0,1]
-}
toNodeNames: TriplesList -> List String
toNodeNames tlist =
  List.map .subject tlist


findByPath : TriplesData -> Pathway -> String
findByPath model pathway =
  String.join "--" pathway
