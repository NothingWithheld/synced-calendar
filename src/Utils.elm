module Utils exposing (..)

import Flip


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
