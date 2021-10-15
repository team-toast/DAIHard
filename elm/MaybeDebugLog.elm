module MaybeDebugLog exposing (maybeDebugLog)

-- Changed depending on whether building for prod or debug


maybeDebugLog s a =
    -- Debug.log s a
    a