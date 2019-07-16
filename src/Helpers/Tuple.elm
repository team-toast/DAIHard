module Helpers.Tuple exposing (mapTuple2)


mapTuple2 : (a -> b) -> ( a, a ) -> ( b, b )
mapTuple2 f ( first, second ) =
    ( f first
    , f second
    )
