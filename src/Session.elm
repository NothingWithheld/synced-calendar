module Session exposing
    ( Session
    , WithSession
    , getIsDST
    , getKey
    , getOffset
    , getUserId
    , hasUserId
    , init
    , setUserId
    , signOut
    )

import Browser.Navigation as Nav
import Json.Decode as Decode exposing (Decoder)


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


getIsDST : Session -> Bool
getIsDST (Session _ { isDST } _) =
    isDST


hasUserId : Session -> Bool
hasUserId (Session _ _ userId) =
    userId /= ""
