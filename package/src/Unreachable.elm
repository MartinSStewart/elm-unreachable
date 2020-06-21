module Unreachable exposing (alwaysErr, alwaysJust, alwaysOk, unreachable)

{-| This indicates that a pattern in a case...of expression is unreachable.
`unreachable` must be called directly inside a pattern match and must be fully applied.

    -- Valid
    case Email.parse "example@mail.com" of
        Just email ->
            email

        Nothing ->
            unreachable ()

-}


unreachable : () -> a
unreachable () =
    let
        -- Make sure this won't get TCO'd so we can get a stackoverflow should this ever get called somehow.
        helper : Int -> Int
        helper value =
            helper value + 1
    in
    if helper 0 == 0 then
        unreachable ()

    else
        unreachable ()


{-| Assert that a `Maybe` is always a `Just` value.
-}
alwaysJust : Maybe a -> a
alwaysJust maybe =
    case maybe of
        Just value ->
            value

        Nothing ->
            unreachable ()


{-| Assert that a `Result` is always an `Ok` value.
-}
alwaysOk : Result e a -> a
alwaysOk result =
    case result of
        Ok ok ->
            ok

        Err _ ->
            unreachable ()


{-| Assert that a `Result` is always an `Err` value.
-}
alwaysErr : Result e a -> e
alwaysErr result =
    case result of
        Ok _ ->
            unreachable ()

        Err err ->
            err
