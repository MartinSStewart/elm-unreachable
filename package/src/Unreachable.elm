module Unreachable exposing (alwaysErr, alwaysJust, alwaysOk, unreachableCase)


unreachableCase : () -> a
unreachableCase () =
    unreachableCase ()


alwaysJust : Maybe a -> a
alwaysJust maybe =
    case maybe of
        Just value ->
            value

        Nothing ->
            unreachableCase ()


alwaysOk : Result e a -> a
alwaysOk result =
    case result of
        Ok ok ->
            ok

        Err _ ->
            unreachableCase ()


alwaysErr : Result e a -> e
alwaysErr result =
    case result of
        Ok _ ->
            unreachableCase ()

        Err err ->
            err
