module Ntriples.Filter
    exposing
        (
        unique
        )

{-| Convenience functions for filtering list of ntriples

# Basics
@docs  unique

-}

import List exposing (..)

{-| Remove duplicate values, keeping the first instance of each element which appears more than once.

    unique [0,1,1,0,1] == [0,1]
-}
unique : List comparable -> List comparable
unique list =
    list
