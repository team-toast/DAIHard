module Clock

let trim totalminutes =
    let boundminutes = totalminutes % 1440
    if boundminutes < 0 then 1440 + boundminutes
    else boundminutes

let create hours minutes = (hours * 60 + minutes) |> trim

let add minutes clock = (clock + minutes) |> trim

let subtract minutes clock = add (-minutes) clock

let display clock = sprintf "%02i:%02i" (clock / 60) (clock % 60)
