module Session exposing
    ( Session
    , WithSession
    , getIsDST
    , getKey
    , getOffset
    , getTimeZoneLabels
    , getUserId
    , getZone
    , hasUserId
    , init
    , isDSTLabels
    , labelForTimeZone
    , nonDSTLabels
    , setOffset
    , setUserId
    , signOut
    )

import Browser.Navigation as Nav
import Json.Decode as Decode exposing (Decoder)
import Time exposing (Zone)
import Utils exposing (findFirst)



-- TimeZone is only a proof of concept, not a viable solution


type Session
    = Session Nav.Key TimeZoneOffset String


type alias WithSession a =
    { a
        | session : Session
    }


type alias TimeZoneOffset =
    { offset : Int
    , isDST : Bool
    }


offsetFlagDecoder : Decoder TimeZoneOffset
offsetFlagDecoder =
    Decode.map2 TimeZoneOffset
        (Decode.field "offset" Decode.int)
        (Decode.field "isDST" Decode.bool)


init : Nav.Key -> Decode.Value -> Session
init key offsetFlag =
    case Decode.decodeValue offsetFlagDecoder offsetFlag of
        Ok { offset, isDST } ->
            let
                hourOffset =
                    offset * -1 // 60

                daylightSavingsChange =
                    if isDST then
                        1

                    else
                        0
            in
            -- limit to PST through EST
            if hourOffset >= -8 + daylightSavingsChange && hourOffset <= -5 + daylightSavingsChange then
                Session key { offset = hourOffset, isDST = isDST } ""

            else
                Session key
                    { offset = -6 + daylightSavingsChange
                    , isDST = isDST
                    }
                    ""

        Err _ ->
            -- default to CST
            Session key { offset = -6, isDST = False } ""


nonDSTLabels : List ( String, Int )
nonDSTLabels =
    [ ( "PST", -8 ), ( "MST", -7 ), ( "CST", -6 ), ( "EST", -5 ) ]


isDSTLabels : List ( String, Int )
isDSTLabels =
    [ ( "PDT", -7 ), ( "MDT", -6 ), ( "CDT", -5 ), ( "EDT", -4 ) ]


labelForTimeZone : Session -> String
labelForTimeZone (Session _ { offset, isDST } _) =
    if isDST then
        findFirst ((==) offset << Tuple.second) isDSTLabels
            |> Maybe.map Tuple.first
            |> Maybe.withDefault "CDT"

    else
        findFirst ((==) offset << Tuple.second) nonDSTLabels
            |> Maybe.map Tuple.first
            |> Maybe.withDefault "CST"


getTimeZoneLabels : Session -> List String
getTimeZoneLabels (Session _ { isDST } _) =
    if isDST then
        List.map Tuple.first isDSTLabels

    else
        List.map Tuple.first nonDSTLabels


signOut : Session -> Session
signOut (Session key timeZone _) =
    Session key timeZone ""


getUserId : Session -> String
getUserId (Session _ _ userId) =
    userId


setUserId : Session -> String -> Session
setUserId (Session key offset _) userId =
    Session key offset userId


getKey : Session -> Nav.Key
getKey (Session key _ _) =
    key


getOffset : Session -> Int
getOffset (Session _ { offset } _) =
    offset


getZone : Session -> Zone
getZone (Session _ { offset } _) =
    Time.customZone (offset * 60) []


setOffset : Session -> String -> Maybe Session
setOffset (Session key { isDST } userId) toTimeZoneLabel =
    let
        newOffset =
            findFirst
                ((==) toTimeZoneLabel << Tuple.first)
                (nonDSTLabels ++ isDSTLabels)
                |> Maybe.map Tuple.second

        mapFunc offset =
            Session key { offset = offset, isDST = isDST } userId
    in
    Maybe.map mapFunc newOffset



-- setOffset : Session -> Int -> Maybe Session
-- setOffset (Session key { isDST } userId) toOffset =
--     let
--         daylightSavingsChange =
--             if isDST then
--                 1
--             else
--                 0
--     in
--     if toOffset >= -8 + daylightSavingsChange && toOffset <= -5 + daylightSavingsChange then
--         Just <| Session key { offset = toOffset, isDST = isDST } userId
--     else
--         Nothing


getIsDST : Session -> Bool
getIsDST (Session _ { isDST } _) =
    isDST


hasUserId : Session -> Bool
hasUserId (Session _ _ userId) =
    userId /= ""
