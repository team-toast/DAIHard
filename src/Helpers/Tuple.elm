module Helpers.Tuple exposing (extractTuple3Result, mapEachTuple3, mapTuple2, mapTuple3, tuple3First, tuple3MapFirst, tuple3MapSecond, tuple3MapThird, tuple3Second, tuple3Third)


mapTuple2 : (a -> b) -> ( a, a ) -> ( b, b )
mapTuple2 f ( first, second ) =
    ( f first
    , f second
    )


mapTuple3 : (a -> b) -> ( a, a, a ) -> ( b, b, b )
mapTuple3 f ( first, second, third ) =
    ( f first
    , f second
    , f third
    )


extractTuple3Result : ( Result a b, Result a b, Result a b ) -> Result a ( b, b, b )
extractTuple3Result ( first, second, third ) =
    case ( first, second, third ) of
        ( Ok fo, Ok so, Ok to ) ->
            Ok ( fo, so, to )

        ( Err fe, _, _ ) ->
            Err fe

        ( _, Err se, _ ) ->
            Err se

        ( _, _, Err te ) ->
            Err te


mapEachTuple3 : (a -> x) -> (b -> x_) -> (c -> x__) -> ( a, b, c ) -> ( x, x_, x__ )
mapEachTuple3 f1 f2 f3 ( v1, v2, v3 ) =
    ( f1 v1
    , f2 v2
    , f3 v3
    )


tuple3MapFirst f =
    mapEachTuple3 f identity identity


tuple3MapSecond f =
    mapEachTuple3 identity f identity


tuple3MapThird f =
    mapEachTuple3 identity identity f


tuple3First : ( a, b, c ) -> a
tuple3First ( a, b, c ) =
    a


tuple3Second : ( a, b, c ) -> b
tuple3Second ( a, b, c ) =
    b


tuple3Third : ( a, b, c ) -> c
tuple3Third ( a, b, c ) =
    c
