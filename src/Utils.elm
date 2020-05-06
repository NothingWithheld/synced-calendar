module Utils exposing (..)

import Flip
import Json.Decode as Decode exposing (Decoder)
import Material


type NoData
    = NoData


noDataDecoder : Decoder NoData
noDataDecoder =
    Decode.succeed NoData


type alias WithMdc msg a =
    { a
        | mdc : Material.Model msg
    }


findFirst : (a -> Bool) -> List a -> Maybe a
findFirst predicate list =
    List.head <| List.filter predicate list


applicative : Maybe (a -> b) -> Maybe a -> Maybe b
applicative =
    Maybe.map2 (<|)


defaultWithoutData : ( a, b ) -> Maybe c -> (c -> ( a, b )) -> ( a, b )
defaultWithoutData default maybeData mapFunc =
    case maybeData of
        Just data ->
            mapFunc data

        Nothing ->
            default


defaultOnError : ( a, b ) -> Result e c -> (c -> ( a, b )) -> ( a, b )
defaultOnError default result mapFunc =
    case result of
        Ok data ->
            mapFunc data

        Err _ ->
            default


getListItemAt : Int -> List a -> Maybe a
getListItemAt index list =
    let
        getListItemAtHelper curList curIndex =
            case curList of
                [] ->
                    Nothing

                head :: rest ->
                    if curIndex == index then
                        Just head

                    else
                        getListItemAtHelper rest (curIndex + 1)
    in
    getListItemAtHelper list 0


useWithoutCmdMsg : (a -> b) -> (a -> ( b, Cmd msg ))
useWithoutCmdMsg fn =
    Flip.flip Tuple.pair Cmd.none << fn


limitToNChars : Int -> String -> String
limitToNChars n input =
    if String.length input <= n then
        input

    else
        String.left n input ++ "..."


getMinMax : Int -> Int -> ( Int, Int )
getMinMax a b =
    if a < b then
        ( a, b )

    else
        ( b, a )


applyTwice : (a -> a -> b) -> a -> b
applyTwice fn arg =
    fn arg arg


maybeMap6 : (a -> b -> c -> d -> e -> f -> g) -> Maybe a -> Maybe b -> Maybe c -> Maybe d -> Maybe e -> Maybe f -> Maybe g
maybeMap6 mapFunc a b c d e f =
    let
        flippedApplicative =
            Flip.flip applicative
    in
    Maybe.map mapFunc a
        |> flippedApplicative b
        |> flippedApplicative c
        |> flippedApplicative d
        |> flippedApplicative e
        |> flippedApplicative f


maybeMap7 : (a -> b -> c -> d -> e -> f -> g -> h) -> Maybe a -> Maybe b -> Maybe c -> Maybe d -> Maybe e -> Maybe f -> Maybe g -> Maybe h
maybeMap7 mapFunc a b c d e f g =
    let
        flippedApplicative =
            Flip.flip applicative
    in
    Maybe.map mapFunc a
        |> flippedApplicative b
        |> flippedApplicative c
        |> flippedApplicative d
        |> flippedApplicative e
        |> flippedApplicative f
        |> flippedApplicative g
