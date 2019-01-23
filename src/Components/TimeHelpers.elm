module TimeHelpers exposing (add, isNegative, sub)

import Time


add : Time.Posix -> Time.Posix -> Time.Posix
add t1 t2 =
    t1
        |> Time.posixToMillis
        |> (+) (Time.posixToMillis t2)
        |> Time.millisToPosix


sub : Time.Posix -> Time.Posix -> Time.Posix
sub t1 t2 =
    t2
        |> Time.posixToMillis
        |> (-) (Time.posixToMillis t1)
        |> Time.millisToPosix


isNegative : Time.Posix -> Bool
isNegative t =
    Time.posixToMillis t < 0
