module Unreachable exposing (alwaysErr, alwaysJust, alwaysOk, unreachableCase)


unreachableCase : () -> a
unreachableCase () =
    unreachableCase ()


{-| Assert that a `Maybe` is always a `Just` value.
-}
alwaysJust : Maybe a -> a
alwaysJust maybe =
    case maybe of
        Just value ->
            value

        Nothing ->
            unreachableCase ()


{-| Assert that a `Result` is always an `Ok` value.
-}
alwaysOk : Result e a -> a
alwaysOk result =
    case result of
        Ok ok ->
            ok

        Err _ ->
            unreachableCase ()


{-| Assert that a `Result` is always an `Err` value.
-}
alwaysErr : Result e a -> e
alwaysErr result =
    case result of
        Ok _ ->
            unreachableCase ()

        Err err ->
            err
